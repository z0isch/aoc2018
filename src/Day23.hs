{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Day23 where
import           Control.Monad
import           Data.Foldable
import           Data.Function
import           Data.HashSet               (HashSet)
import qualified Data.HashSet               as HS
import           Data.Maybe
import           Data.SBV                   (SBV)
import qualified Data.SBV                   as SBV
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Linear.V3
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

sbvManhattenDist :: C -> SBV Integer -> SBV Integer -> SBV Integer -> SBV Integer
sbvManhattenDist (V3 x y z) x1 y1 z1 = absoluteValue (x0 - x1) + absoluteValue (y0 - y1) + absoluteValue (z0 - z1)
    where
        x0 = SBV.literal x
        y0 = SBV.literal y
        z0 = SBV.literal z
        absoluteValue n = SBV.ite (n SBV..< 0) (negate n) n

inRange :: SBV Integer -> SBV Integer -> SBV Integer -> Bot -> SBV Integer
inRange x y z (c,r) = SBV.oneIf $ mDist SBV..<= SBV.literal r
  where
    mDist =  sbvManhattenDist c x y z

part2Sol' :: HashSet Bot -> IO SBV.OptimizeResult
part2Sol' hs = SBV.optimize SBV.Lexicographic $ do
    [x, y, z] <- SBV.sIntegers ["x", "y", "z"]
    SBV.maximize "in range of most bots" . sum $ map (inRange x y z) $ HS.toList hs
    SBV.minimize "distance to 0" $ sbvManhattenDist 0 x y z

part2' :: IO SBV.OptimizeResult
part2' = parseInput >>= part2Sol'

test :: HashSet Bot
test = fromJust $ parseMaybe totP $ "pos=<10,12,12>, r=2\npos=<12,14,12>, r=2\npos=<16,12,12>, r=4\npos=<14,14,14>, r=6\npos=<50,50,50>, r=200\npos=<10,10,10>, r=5"
