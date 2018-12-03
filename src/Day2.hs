{-# LANGUAGE OverloadedStrings #-}
module Day2 where

import           Data.List
import           Data.Maybe
import qualified Data.Set     as S
import           Data.Text    (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

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

combinationsWith :: (a -> a -> b) -> [a] -> [b]
combinationsWith _ []     = []
combinationsWith f (x:xs) = map (f x) xs ++ combinationsWith f xs

findDiff :: [String] -> [String]
findDiff = catMaybes . combinationsWith foo
    where
        foo x y =
            if length ne == 1
            then Just $ map fst equal
            else Nothing
            where (ne,equal) = partition (\(t,v) -> t /= v) $ zip x y


test = ["abcdef","bababc","abbcde","abcccd","aabcdd","abcdee","ababab"]
test2 = ["abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz"]
