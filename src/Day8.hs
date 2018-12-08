{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day8 where
import Data.Void
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer
import Control.Monad
import Data.Monoid
import Safe

data T = T [T] [Integer]
    deriving (Eq, Show)

parseInput :: IO T
parseInput = parse tP "" <$> TIO.readFile "./input/day8.txt" >>= \case
    Left err -> error (parseErrorPretty err)
    Right x -> pure x
    
tP :: Parsec Void Text T
tP = do
    (numChild, numMeta) <- (,) <$> decimal <* " " <*> decimal <* " "
    children <- count numChild tP
    metaData <- count numMeta (decimal <* optional " ") 
    pure $ T children metaData
      
metadatas :: T -> [Integer]
metadatas (T [] md) = md
metadatas (T ts md) = concatMap metadatas ts ++ md

part1Sol :: T -> Integer
part1Sol = getSum . foldMap Sum . metadatas

val :: T -> [Integer]
val (T [] md) = md
val (T ts md) = concatMap getVals md
    where 
        getVals i = case atMay ts (fromIntegral i - 1) of
            Nothing -> [0]
            (Just t) -> val t
 
part2Sol :: T -> Integer
part2Sol = getSum . foldMap Sum . val

part1 :: IO Integer
part1=part1Sol <$> parseInput
part2 :: IO Integer
part2=part2Sol <$> parseInput

test :: Maybe T
test = parseMaybe tP $ "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"