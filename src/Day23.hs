{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day23 where

import Linear.V3
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import           Data.Text                  (Text)
import qualified           Data.Text                  as T
import qualified Data.Text.IO               as TIO
import Control.Monad
import Data.Foldable
import Data.Function
import qualified Data.HashSet as HS
import Data.HashSet (HashSet)

type C = V3 Integer
type P = Parsec Void Text
type Bot = (C,Integer)

parseInput :: IO (HashSet Bot)
parseInput = parse totP "" <$> TIO.readFile "./input/day23.txt" >>= \case
    Left err -> error (parseErrorPretty err)
    Right x -> pure x

totP :: P (HashSet Bot)
totP = HS.fromList <$> some (bP <* (void eol <|> eof))
bP = (,) <$ "pos=" <*> cP <* ", r=" <*> L.decimal
cP = between "<" ">" $ (\[x,y,z] -> V3 x y z) <$> (L.signed (void "") L.decimal) `sepBy` "," 

manhattenDist :: C -> C -> Integer
manhattenDist (V3 x1 y1 z1) (V3 x2 y2 z2) = abs (x1 - x2) + abs (y1 - y2) +abs (z1 - z2) 

part1Sol hs = let (c1,r) = maximumBy (compare `on` snd) hs in length $ HS.filter (\(c2,_) -> manhattenDist c1 c2 <= r) hs

part1 :: IO Int
part1 = part1Sol <$> parseInput