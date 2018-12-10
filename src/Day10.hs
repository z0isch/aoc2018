{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Day10 where

import           Codec.Picture
import           Control.Monad
import           Data.List
import           Data.Maybe
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data C = C {cX,cY :: !Int}
  deriving (Eq,Show)

data L = L {lPos, lVel :: !C }
  deriving (Eq,Show)

type B = (C,C)

parseInput :: IO [L]
parseInput = parse totP "" <$> TIO.readFile "./input/day10.txt" >>= \case
    Left err -> error (parseErrorPretty err)
    Right x -> pure x

totP :: Parsec Void Text [L]
totP = many (cvP <* (void eol <|> eof))

cvP :: Parsec Void Text L
cvP = L <$ "position=" <*> angleP <* " velocity=" <*> angleP

angleP ::  Parsec Void Text C
angleP = between "<" ">" $ C <$> dP  <* "," <*> dP
  where dP = void (optional space) *> L.signed space L.decimal

move :: L -> L
move (L (C x y) (v@(C vx vy))) = L (C (x+vx) (y+vy)) v

gettingSmaller :: [L] -> [L] -> Bool
gettingSmaller before after = area (boundingBox (map lPos before)) >= area (boundingBox (map lPos after))

area :: B -> Int
area ((C x1 y1),(C x2 y2)) = (x2-x1) * (y2-y1)

boundingBox :: [C] -> B
boundingBox cs = (C minX minY,C maxX maxY)
  where
    minX = minimum $ map cX cs
    minY = minimum $ map cY cs
    maxX = maximum $ map cX cs
    maxY = maximum $ map cY cs

mkCoords :: [L] -> [C]
mkCoords ms = map ((\(C x y) -> (C (if minX < 0 then x-minX else x) (if minY < 0 then y-minY else y))) . lPos) ms
  where
    minX = minimum $ map (cX . lPos) ms
    minY = minimum $ map (cY . lPos) ms

genI :: Int -> Int -> [C] -> Image PixelRGB8
genI w h cs = generateImage color w h
  where color x y = if (C x y) `elem` cs then (PixelRGB8 255 255 255) else (PixelRGB8 0 0 0)

doAnimation ::  [L] -> [[L]]
doAnimation ls = whileConverging ++ [lastFrame]
  where
    lastFrame = map move (last whileConverging)
    whileConverging = unfoldr x ls
    x before = if gettingSmaller before after then Just (before, after) else Nothing
      where after = map move before

part1Sol :: [L] -> [Image PixelRGB8]
part1Sol ls = pure $ genI (maxX + 1) (maxY + 1) $ mkCoords lastFrame
  where
    lastFrame = last $ doAnimation ls
    maxX = maximum $ map (cX.lPos) lastFrame
    maxY = maximum $ map (cY.lPos) lastFrame

writeImg :: FilePath -> [L] -> IO ()
writeImg fn i = case (writeGifAnimation fn 1 LoopingForever $ part1Sol i) of
  Left str -> print str
  Right a  -> a

part1= parseInput >>= writeImg "day10.gif"

part2= (\x -> x -1) . length . doAnimation <$> parseInput

test :: [L]
test = fromJust $ parseMaybe totP "position=< 9,  1> velocity=< 0,  2>\nposition=< 7,  0> velocity=<-1,  0>\nposition=< 3, -2> velocity=<-1,  1>\nposition=< 6, 10> velocity=<-2, -1>\nposition=< 2, -4> velocity=< 2,  2>\nposition=<-6, 10> velocity=< 2, -2>\nposition=< 1,  8> velocity=< 1, -1>\nposition=< 1,  7> velocity=< 1,  0>\nposition=<-3, 11> velocity=< 1, -2>\nposition=< 7,  6> velocity=<-1, -1>\nposition=<-2,  3> velocity=< 1,  0>\nposition=<-4,  3> velocity=< 2,  0>\nposition=<10, -3> velocity=<-1,  1>\nposition=< 5, 11> velocity=< 1, -2>\nposition=< 4,  7> velocity=< 0, -1>\nposition=< 8, -2> velocity=< 0,  1>\nposition=<15,  0> velocity=<-2,  0>\nposition=< 1,  6> velocity=< 1,  0>\nposition=< 8,  9> velocity=< 0, -1>\nposition=< 3,  3> velocity=<-1,  1>\nposition=< 0,  5> velocity=< 0, -1>\nposition=<-2,  2> velocity=< 2,  0>\nposition=< 5, -2> velocity=< 1,  2>\nposition=< 1,  4> velocity=< 2,  1>\nposition=<-2,  7> velocity=< 2, -2>\nposition=< 3,  6> velocity=<-1, -1>\nposition=< 5,  0> velocity=< 1,  0>\nposition=<-6,  0> velocity=< 2,  0>\nposition=< 5,  9> velocity=< 1, -2>\nposition=<14,  7> velocity=<-2,  0>\nposition=<-3,  6> velocity=< 2, -1>"
