{-# LANGUAGE OverloadedStrings #-}
module Day3 where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.Text (Text)
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char

data R = R Integer (Integer, Integer) Integer Integer
    deriving (Eq, Show)

getInput :: IO [Text]
getInput = T.lines <$> TIO.readFile "./input/day3.txt"

rParser :: Parsec Void Text R
rParser = R <$> idParser <*> coordP <*> hParser <*> wParser
    where
        idParser = read <$> char '#' *> manyTill numberChar spaceChar
        coordP = (,) <$> _a <*> _b
        hParser = _c
        wParser = _d


part1 = undefined

part2 = undefined