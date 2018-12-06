{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Day6 where

import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

data C = C {cX, cY :: !Integer}
  deriving (Eq, Show, Ord)
data B = B !C !C
  deriving (Eq, Show, Ord)

parseInput :: IO [C]
parseInput = parse totP "" <$> TIO.readFile "./input/day6.txt" >>= \case
    Left err -> error (parseErrorPretty err)
    Right x -> pure x

totP :: Parsec Void Text [C]
totP = many (coordP <* (void eol <|> eof))

coordP :: Parsec Void Text C
coordP = C <$> decimal <* ", " <*> decimal

manhattenDist :: C -> C -> Integer
manhattenDist (C x1 y1) (C x2 y2) = abs (y1 - y2) + (abs (x1 - x2))

boundingBox :: [C] -> B
boundingBox cs = B (C (minimum xs) (minimum ys)) (C (maximum xs) (maximum ys))
  where
    xs = map cX cs
    ys = map cY cs

inBox :: B -> C -> Bool
inBox (B (C minX minY) (C maxX maxY)) (C x y) = x > minX && x < maxX && y > minY && y < maxY

candidatePts :: [C] -> [C]
candidatePts cs = filter (inBox (boundingBox cs)) cs

ptsInBox :: B -> [C]
ptsInBox (B (C minX minY) (C maxX maxY)) = concatMap (\x -> map (C x) [minY+1..maxY-1]) [minX+1..maxX-1]

minimumsBy :: (Eq a, Ord b) => (a -> b) -> [a] -> [a]
minimumsBy f xs = map fst $ head $ groupBy ((==) `on` snd) $ sortOn snd $ map (\x -> (x, f x)) xs

closestPts :: [C] -> C -> [C]
closestPts cs c = minimumsBy (manhattenDist c) cs

closestCandidatePts :: [C] -> C -> [C]
closestCandidatePts cs = filter (`elem` candidatePts cs) . closestPts cs

histogram :: (Eq a, Ord a) => [a] -> [(a,Integer)]
histogram = map (\(x:xs) -> (x, genericLength xs)) . group. sort

part1Sol :: [C] -> Integer
part1Sol cs = snd
  $ maximumBy (compare `on` snd)
  $ histogram
  $ concat
  $ filter ((==) 1 . length)
  $ map (closestCandidatePts cs)
  $ ptsInBox
  $ boundingBox cs

part2Sol :: Integer -> [C] -> Int
part2Sol m cs = length
  $ filter (< m)
  $ map (\p -> sum $ map (manhattenDist p) cs)
  $ ptsInBox
  $ boundingBox cs

part1 :: IO Integer
part1 = part1Sol <$> parseInput

part2 :: IO Int
part2 = part2Sol 10000 <$> parseInput

test :: [C]
test = fromJust $ parseMaybe totP "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"
