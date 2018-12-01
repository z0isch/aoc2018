{-# LANGUAGE OverloadedStrings #-}
module Day1 where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import qualified Data.Set as S
import Data.List (foldl')

getInput :: IO [Integer]
getInput = map (read . filter (/= '+') . T.unpack) . T.lines <$> TIO.readFile "./input/day1.txt"

part1 :: IO Integer
part1 = sum <$> getInput

part2 :: IO Integer
part2 = getRepeat (S.singleton 0) 0 . cycle <$> getInput
    where
        getRepeat s t (x:xs)
            | t' `S.member` s = t'
            | otherwise = getRepeat (S.insert t' s) t' xs
            where t' = t + x
