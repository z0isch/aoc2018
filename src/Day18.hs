{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Day18 where

import           Data.Bool
import qualified Data.Map.Strict   as M
import           Data.Massiv.Array (Ix2 (..))
import qualified Data.Massiv.Array as A
import qualified Data.Sequence     as S
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

--0 Empty
--1 Tree
--2 Lumberyard
type Acre = Int

tP :: Int -> String -> A.Array A.S Ix2 Acre
tP s = acreArr s . map (map aP) . lines

aP :: Char -> Acre
aP '.' = 0
aP '|' = 1
aP '#' = 2
aP _   = error "bad"

acreArr :: Int -> [[Acre]] -> A.Array A.S A.Ix2 Acre
acreArr s m = A.makeArray A.Par (s A.:. s) (\(i A.:. j) -> m !! i !! j)

stepStencil :: A.Stencil A.Ix2 Acre Acre
stepStencil = A.makeStencil (A.Fill (-1)) (3 :. 3) (1 :. 1) getRules
  where
    getRules :: (A.Ix2 -> A.Value Acre) -> A.Value Acre
    getRules g = acreRules <$> adjacents <*> g (0:.0)
      where adjacents = filter (>= 0) <$> traverse g [x:.y | x <- [-1,0,1], y <- [-1,0,1] , not (x == 0 && y == 0)]

acreRules :: [Acre] -> Acre -> Acre
acreRules adjs = \case
  -1 -> -1
  0  -> bool 0 1 $ moreThan 2 1 adjs
  1  -> bool 1 2 $ moreThan 2 2 adjs
  2  -> bool 0 2 $ (\x -> moreThan 0 1 x && moreThan 0 2 x) adjs
  where moreThan x p = (> x) . length . filter (== p)

applyStencil :: A.Array A.S Ix2 Acre -> A.Array A.S Ix2 Acre
applyStencil =  A.compute . A.mapStencil stepStencil

sol :: Int -> A.Array A.S A.Ix2 Acre -> Int
sol n i = length (filter (== 1) $ final) * length (filter (== 2) final)
    where
      final = A.toList $ head $ drop n $ iterate applyStencil i

sol' :: Int -> A.MArray A.RealWorld A.S A.Ix2 Acre -> IO Int
sol' n mArr = do
    initial <- A.freeze A.Par mArr
    go 0 (M.singleton initial 0)
      where
        answer final = length (filter (== 1) $ final) * length (filter (== 2) final)
        go i m
          | n == i = answer . A.toList <$> A.freeze A.Par mArr
          | otherwise = do
            next <- applyStencil <$> A.freeze A.Par mArr
            if next `M.member` m
              then
                let r = m M.! next
                    --Repeats between r and i, so we can index what we already have
                    idx = r + ((n - r) `mod` ((i+1) - r))
                in pure $ answer $ A.toList $ fst $ head $ filter ((== idx).snd) $ M.toList m
              else do
                let m' = M.insert next (i+1) m
                A.imapP_ (\idx -> A.write' mArr idx) next
                go (i+1) m'

part1 :: IO Int
part1 = do
  m <- tP 50 . T.unpack <$> TIO.readFile "./input/Day18.txt"
  A.thaw m >>= sol' 10

part2 :: IO Int
part2 = do
  m <- tP 50 . T.unpack <$> TIO.readFile "./input/Day18.txt"
  A.thaw m >>= sol' 1000000000

printA :: Int -> A.Array A.B Ix2 Acre -> IO ()
printA i = mapM_ putStrLn . showA i
  where
    showA s = fmap (foldMap id) . S.chunksOf s . foldMap (pure . showNum)
    showNum 0 = "."
    showNum 1 = "|"
    showNum 2 = "#"

test :: A.Array A.S Ix2 Acre
test = tP 10 ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|."
