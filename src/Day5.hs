{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import           Data.Char
import           Data.Either
import           Data.List
import           Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

input :: IO Text
input = TIO.readFile "./input/day5.txt"

totP :: Parsec Void Text Text
totP = T.pack . catMaybes <$> many (try (const Nothing <$> doubleP) <|> (Just <$> letterChar))

doubleP :: Parsec Void Text (Char, Char)
doubleP = do
  x <- letterChar
  let opposite = if isUpper x then toLower x else toUpper x
  y <- char opposite
  pure $ (x,y)

filterUnit :: Text -> Char -> Text
filterUnit xs c = T.filter (\x -> not (x == c || x == toUpper c)) xs

doRounds :: Text -> Either Text Text
doRounds st = go mempty st
  where
    go xs ys
      | T.length xs == T.length ys = Right ys
      | otherwise = case parse totP "" ys of
          Left err  -> Left $ T.pack $ parseErrorPretty err
          Right ys' -> go ys ys'

filterThenDoRounds :: Text -> [Either Text Text]
filterThenDoRounds xs = map (doRounds . filterUnit xs) letters
    where
      letters = filter isLower $ map head $ group $ sort $ T.unpack xs

part1Sol :: Text -> Either Text Int
part1Sol = fmap T.length . doRounds

part2Sol :: Text -> Int
part2Sol = minimum . rights . map (fmap T.length) . filterThenDoRounds

part1 :: IO (Either Text Int)
part1 = part1Sol <$> input
part2 :: IO Int
part2 = part2Sol <$> input

test :: Text
test = "dabAcCaCBAcCcaDA"
