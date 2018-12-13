{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE TypeOperators  #-}
module Day12 where

import           Control.Comonad
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

type Rule = [Bool]

rules :: IO (Map Rule Bool)
rules = parseRules . T.unpack <$> TIO.readFile "./input/day11.txt"

initialState :: [Bool]
initialState = parsePots "#.#.#..##.#....#.#.##..##.##..#..#...##....###..#......###.#..#.....#.###.#...#####.####...#####.#.#"

parsePots :: [Char] -> [Bool]
parsePots = map potP

parseRules :: String -> Map Rule Bool
parseRules = M.fromList . map parseRule . lines

parseRule :: String -> (Rule, Bool)
parseRule cs = (map potP r, t)
  where
    r = takeWhile (/= ' ') cs
    t = p $ dropWhile (/= ' ') cs
    p (' ':'=':'>':' ':x:[]) = potP x

potP :: Char -> Bool
potP '.' = False
potP '#' = True

data S = S {sP :: !Integer, sB :: !Bool}
  deriving (Eq,Show)

data U a = U [a] a [a]
    deriving (Eq, Show, Functor, Foldable)

instance Comonad U where
  extract :: U a -> a
  extract (U _ a _) = a
  duplicate :: U a -> U (U a)
  duplicate a = U (gather left) a (gather right)
    where
      gather f = tail $ iterate f a

right :: U a -> U a
right (U a b (c:cs))  = U (b:a) c cs

left :: U a -> U a
left (U (a:as) b cs) = U as a (b:cs)

rule :: Map [Bool] Bool -> U S -> S
rule rs (U ((S _ b):(S _ a):_) (S i c) ((S _ d):(S _ e):_)) = S i (fromMaybe False $ [a,b,c,d,e] `M.lookup` rs)

applyRule :: Map [Bool] Bool -> U S -> [U S]
applyRule rs = iterate $ extend (rule rs)

makeU :: [Bool] -> U S
makeU (b:bs) = U behind (S 0 b) after
  where
    behind = zipWith S [(-1),(-2)..] (repeat False)
    after = zipWith S [1..] (bs ++ repeat False)

window :: (Integer,Integer) -> U S -> [S]
window (x,y) (U as b cs) = genericTake x as ++ b:genericTake y cs

part1Sol :: Map [Bool] Bool -> [Bool] -> Integer -> Sum Integer
part1Sol rs i steps = foldMap summer
  $ window (steps,genericLength i+steps)
  $ last
  $ genericTake (steps + 1)
  $ applyRule rs
  $ makeU i
    where
      summer (S _ False)  = mempty
      summer (S idx True) = Sum idx

printGens :: Map [Bool] Bool -> [Bool] -> Integer -> [String]
printGens rs i steps = map (prettyPrintS' . window (steps,genericLength i+steps)) $ genericTake (steps + 1) $ applyRule rs $ makeU i

printGens' :: Map [Bool] Bool -> [Bool] -> Integer -> [String]
printGens' rs i steps = map (dropWhile ((==) '.')) $ map (prettyPrintS . window (steps,genericLength i+steps)) $ genericTake (steps + 1) $ applyRule rs $ makeU i

prettyPrintS' :: [S] -> String
prettyPrintS' = concatMap (\(S i b) -> show i ++ if b then "#" else ".")

prettyPrintS :: [S] -> String
prettyPrintS = map (\(S _ b) -> if b then '#' else '.')

printPart1' :: Integer -> IO ()
printPart1' i = do
  rs <- rules
  putStrLn $ last $ printGens rs initialState i

printPart1 :: Integer -> IO ()
printPart1 i = do
  rs <- rules
  mapM_ putStrLn $ printGens rs initialState i

part1 :: IO (Sum Integer)
part1 = do
  rs <- rules
  pure $ part1Sol rs initialState 20

part2 :: Sum Integer
part2 = magic 50000000000

--98 position 72
magic :: (Enum a, Num a) => a -> Sum a
magic i = foldMap (Sum . fst) $ filter snd $ zip [(i-26)..] $ parsePots stableState

stableState :: String
stableState =  "#...#......#.....#...#.........#...#.....#...#...#....#.......#.....#.....#.....#.....#...#...#...#...#...#.....#...#...#....#."

testMagic :: Integer -> IO ()
testMagic i = do
  rs <- rules
  let actual = part1Sol rs initialState i
  let testOne = magic i
  print actual
  print testOne

runTest :: [U S]
runTest = applyRule testRules $ makeU $ head test

testWorks :: [Bool]
testWorks = zipWith (==) test $ map (map sB . window (0,38)) runTest

test :: [[Bool]]
test = map parsePots $ lines "...#..#.#..##......###...###...........\n...#...#....#.....#..#..#..#...........\n...##..##...##....#..#..#..##..........\n..#.#...#..#.#....#..#..#...#..........\n...#.#..#...#.#...#..#..##..##.........\n....#...##...#.#..#..#...#...#.........\n....##.#.#....#...#..##..##..##........\n...#..###.#...##..#...#...#...#........\n...#....##.#.#.#..##..##..##..##.......\n...##..#..#####....#...#...#...#.......\n..#.#..#...#.##....##..##..##..##......\n...#...##...#.#...#.#...#...#...#......\n...##.#.#....#.#...#.#..##..##..##.....\n..#..###.#....#.#...#....#...#...#.....\n..#....##.#....#.#..##...##..##..##....\n..##..#..#.#....#....#..#.#...#...#....\n.#.#..#...#.#...##...#...#.#..##..##...\n..#...##...#.#.#.#...##...#....#...#...\n..##.#.#....#####.#.#.#...##...##..##..\n.#..###.#..#.#.#######.#.#.#..#.#...#..\n.#....##....#####...#######....#.#..##."

testRules :: Map Rule Bool
testRules = parseRules "...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #"


