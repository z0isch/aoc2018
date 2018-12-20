module Day14 where

import           Data.List
import           Data.Sequence    (Seq)
import qualified Data.Sequence    as S
import           Text.Regex.Posix ((=~))

data S = S
  { sS  :: !(Seq Char)
  , sE1 :: !Int
  , sE2 :: !Int
  }
  deriving (Eq, Show)

doStep :: S -> S
doStep (S s x y) = S s' x' y'
  where
    cX = read [S.index s x]
    cY = read [S.index s y]
    s' = foldl' (S.|>) s $ show $ cX + cY
    x' = (cX + 1 + x) `mod` S.length s'
    y' = (cY + 1 + y) `mod` S.length s'

initial :: S
initial = S (S.fromList "37") 0 1

part1Sol :: Int -> String
part1Sol i = foldMap pure
  $ S.take 10
  $ S.drop i
  $ last
  $ takeWhile ((< (11 + i)) . S.length)
  $ map sS
  $ iterate doStep initial

part1 :: String
part1 = part1Sol 846021

contains :: Seq Char -> Seq Char -> Maybe Int
contains is s =
  if diffTill > subList
  then Nothing
  else Just diffTill
  where
    subList = S.length s - S.length is
    diffTill = S.length
      $ S.takeWhileL (\xs -> is /= S.take (S.length is) xs)
      $ S.take (subList + 1)
      $ S.tails s

part2Sol :: String -> Maybe Int
part2Sol i = contains (S.fromList i)
  $ sS
  $ head
  $ dropWhile (not . (=~ i) . sS)
  $ iterate doStep initial

part2 :: Maybe Int
part2= part2Sol "846021"
