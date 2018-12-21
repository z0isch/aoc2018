{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ViewPatterns        #-}
module Day14 where

import           Data.List
import           Data.Sequence    (Seq)
import qualified Data.Sequence    as S
import           Text.Regex.Posix ((=~))
import Data.Monoid

chocolatePractice :: [Int]
chocolatePractice = 3 : 7 : go 0 1 (S.fromList [3,7])
  where
    go !p1 !p2 !tp = newDigits ++ go p1' p2' tp'
      where
        sc1 = tp `S.index` p1
        sc2 = tp `S.index` p2
        newDigits = digitize $ sc1 + sc2
        tp' = tp <> S.fromList newDigits
        p1' = (p1 + sc1 + 1) `mod` length tp'
        p2' = (p2 + sc2 + 1) `mod` length tp'

digitize :: Int -> [Int]
digitize ((`divMod` 10)->(x,y))
    | x == 0    = [y]
    | otherwise = [x,y]

day14a :: Int -> [Int]
day14a n = take 10 (drop n chocolatePractice)

substrLoc xs = length
             . takeWhile (not . (xs `isPrefixOf`))
             . tails

--day14b :: [Int] -> [Int]
day14b xs = xs `substrLoc` chocolatePractice

data S = S
  { sS  :: !(Seq Int)
  , sE1 :: !Int
  , sE2 :: !Int
  }
  deriving (Eq, Show)

doStep :: S -> S
doStep (S s x y) = S s' x' y'
  where
    cX = S.index s x
    cY = S.index s y
    digitized = 
      let (x,y) = (cX + cY) `divMod` 10
      in if x == 0 then [y] else [x,y]
    s' = foldl' (S.|>) s digitized
    x' = (cX + 1 + x) `mod` S.length s'
    y' = (cY + 1 + y) `mod` S.length s'

initial :: S
initial = S (S.fromList [3,7]) 0 1

part1Sol :: Int -> String
part1Sol i = foldMap show
  $ S.take 10
  $ S.drop i
  $ last
  $ takeWhile ((<= (10 + i)) . S.length)
  $ map sS
  $ iterate doStep initial

--part1 :: String
part1 = part1Sol 846021

-- contains :: Seq Char -> Seq Char -> Maybe Int
-- contains is s =
--   if diffTill > subList
--   then Nothing
--   else Just diffTill
--   where
--     subList = S.length s - S.length is
--     diffTill = S.length
--       $ S.takeWhileL (\xs -> is /= S.take (S.length is) xs)
--       $ S.take (subList + 1)
--       $ S.tails s

-- --part2Sol :: String -> Maybe Int
-- part2Sol i = contains (S.fromList i)
--   $ _a
--   $ head
--   $ dropWhile (not . (=~ i) . show . sS)
--   $ iterate doStep initial

-- part2 :: Maybe Int
-- part2= part2Sol "846021"
