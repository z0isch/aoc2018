{-# LANGUAGE OverloadedStrings #-}
module Day2 where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Set as S
import Data.List

getInput :: IO [String]
getInput = map T.unpack . T.lines <$> TIO.readFile "./input/day2.txt"

part1 :: IO Int
part1 = uniqueLetters <$> getInput

uniqueLetters :: [String] -> Int
uniqueLetters xs = is 2 * is 3
    where 
        is x = length $ filter (== x) tot
        tot = concatMap (nub . filter (/= 1) . map length . group . sort) xs 

part2 :: IO String
part2 = head . findDiff <$> getInput

findDiff :: [String] -> [String]
findDiff xs = do
    x <- xs
    y <- delete x xs
    let (ne,equal) = partition (\(t,v) -> t /= v) $ zip x y
    if length ne == 1 
    then [map fst equal]
    else []

test = ["abcdef","bababc","abbcde","abcccd","aabcdd","abcdee","ababab"]
test2 = ["abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz"]