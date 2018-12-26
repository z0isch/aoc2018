{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Day25 where

import Data.Void
import Control.Monad
import Linear.V4
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS
import Data.Maybe
import Linear.Vector

type C = V4 Int
type P = Parsec Void Text

parseInput :: IO [C]
parseInput = parse totP "" <$> TIO.readFile "./input/day25.txt" >>= \case
    Left err -> error (parseErrorPretty err)
    Right x -> pure x

totP :: P [C] 
totP = cP `sepBy` "\n"
cP =  (\[x,y,z,t] -> V4 x y z t) <$> (L.signed (void "") L.decimal) `sepBy` ","

manhattenD :: C -> C -> Int
manhattenD c1 c2 = sum $ fmap abs $ c1 - c2

sameConst :: C -> HashSet C -> Bool
sameConst c = any ((<= 3) . manhattenD c)

part1Sol :: [C] -> HashSet (HashSet C)
part1Sol (c:cs) = foldr smash (HS.singleton (HS.singleton c)) cs
    where 
        smash c hs = HS.insert newConst $ foldr HS.delete hs sames
            where
                newConst = foldr HS.union (HS.singleton c) sames
                sames = HS.filter (sameConst c) hs 

part1 = length . part1Sol <$> parseInput

test, test2, test3, test4 :: [C]
test = fromJust $ parseMaybe totP "-1,2,2,0\n0,0,2,-2\n0,0,0,-2\n-1,2,0,0\n-2,-2,-2,2\n3,0,2,-1\n-1,3,2,2\n-1,0,-1,0\n0,2,1,-2\n3,0,0,0"
test2 = fromJust $ parseMaybe totP "1,-1,0,1\n2,0,-1,0\n3,2,-1,0\n0,0,3,1\n0,0,-1,-1\n2,3,-2,0\n-2,2,0,0\n2,-2,0,-1\n1,-1,0,-1\n3,2,0,2"
test3 = fromJust $ parseMaybe totP "0,0,0,0\n3,0,0,0\n0,3,0,0\n0,0,3,0\n0,0,0,3\n0,0,0,6\n9,0,0,0\n12,0,0,0"
test4 = fromJust $ parseMaybe totP "1,-1,-1,-2\n-2,-2,0,1\n0,2,1,3\n-2,3,-2,1\n0,2,3,-2\n-1,-1,1,-2\n0,-2,-1,0\n-2,2,3,-1\n1,2,2,0\n-1,-2,0,-2"