{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import           Control.Monad
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type C = (Integer,Integer)

data R = R
    { rId :: Integer
    , rC  :: C
    , rW  :: Integer
    , rH  :: Integer
    }
    deriving (Eq, Show)

parseInput :: IO [R]
parseInput = parse fileParser "" <$> TIO.readFile "./input/day3.txt" >>= \case
    Left err -> error (parseErrorPretty err)
    Right x -> pure x

fileParser :: Parsec Void Text [R]
fileParser = many (rParser <* (void eol <|> eof))

rParser :: Parsec Void Text R
rParser = R
    <$> (char '#' *> decimal)
    <*> (string " @ " *> coordP)
    <* string ": "
    <*> decimal
    <* char 'x'
    <*> decimal
    where
        coordP = (,) <$> decimal <* char ',' <*> decimal

ptsR :: R -> Set C
ptsR (R _ (x, y) w h) = S.fromList [(x', y') | x' <- [x..x+w-1], y' <- [y..y+h-1]]

permWith :: (a -> a -> b) -> [a] -> [b]
permWith _ []     = []
permWith f (x:xs) = map (f x) xs ++ permWith f xs

intersections :: [R] -> Set C
intersections = S.unions . permWith S.intersection . map ptsR

noIntersections :: [R] -> [R]
noIntersections rs = filter (S.null . S.intersection (intersections rs) . ptsR) rs

part1 :: IO Int
part1 = S.size . intersections <$> parseInput

part2 :: IO Integer
part2 = rId . head . noIntersections <$> parseInput

test :: Maybe [R]
test = parseMaybe fileParser "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2\n#4 @ 0,0: 4x4"
