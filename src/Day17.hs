{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day17 where

import           System.IO.Unsafe
import Linear.V2
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text                  (Text)
import qualified           Data.Text                  as T
import qualified Data.Text.IO               as TIO
import           Data.Void
import Control.Monad
import Control.Lens
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Bool
import qualified Data.IntPSQ as Q
import Debug.Trace

data W = Flowing | Stable deriving (Eq, Show)
makePrisms ''W
type C = V2 Int
type R = HashMap C (Maybe W)
type P = Parsec Void Text

parseInput :: IO R
parseInput = parse totP "" <$> TIO.readFile "./input/day17.txt" >>= \case
    Left err -> error (parseErrorPretty err)
    Right x -> pure x

totP :: P R
totP = HM.fromList . ((spring, Just Flowing):) . concat <$> some (cP <* (void eol <|> eof))
cP = (\t x y1 y2 -> map (\y -> if t == 'x' then (V2 x y, Nothing) else (V2 y x, Nothing)) [y1..y2]) 
    <$> oneOf ['x','y'] <* "=" <*> L.decimal 
    <* ", " <* oneOf ['x','y'] <* "=" <*> L.decimal <* ".." <*> L.decimal

spring :: C
spring = V2 500 0

highestY :: R -> Int
highestY = view _y . minimumBy (compare `on` view _y) . filter (\(V2 _ y) -> y > 0) . HM.keys

lowestY :: R -> Int
lowestY = view _y . maximumBy (compare `on` view _y) . HM.keys

part1Sol r = length $ HM.filterWithKey (\(V2 _ y) v -> isJust v && y >= highestY r) $ snd $ doIt (lowestY r) (0,r)
part2Sol r = length $ HM.filterWithKey (\(V2 _ y) v -> isStable v && y >= highestY r) $ snd $ doIt (lowestY r) (0,r)
    where
        isStable (Just Stable) = True
        isStable _ = False 
        
part1 = part1Sol <$> parseInput 
part2 = part2Sol <$> parseInput 

doIt :: Int -> (Int,R) -> (Int,R)
doIt maxDepth (y,r)
    | y == maxDepth = (y,r)
    | otherwise = if a
        then doIt maxDepth ((y-1), r')
        else doIt maxDepth ((y+1), flowEm)
    where
        flowEm = foldr (\(V2 x y) -> HM.insert (V2 x (y+1)) (Just Flowing)) r' $ filter (not . hasGround r') $ canFlowOnY y r'
        (r', a) = foldr (\c (r',a) -> let (r'', a') = fillAtC c r' in (r'',a || a')) (r, False) $ canFlowOnY y r

canFlowOnY :: Int -> R -> [C]
canFlowOnY y r = filter (\c -> atLevel c && isFlowing c) $ HM.keys r
    where
        isFlowing :: C -> Bool
        isFlowing c = case HM.lookup c r of
            Just (Just Flowing) -> True
            _ -> False
        atLevel = (== y) . view _y

hasGround :: R -> C -> Bool
hasGround r (V2 x y) = case HM.lookup (V2 x (y+1)) r of 
    Just (Just Stable) -> True
    Just Nothing -> True
    _ -> False

fillAtC :: C -> R -> (R,Bool)
fillAtC c@(V2 x y) r
    | hasGround r c = (doSide right $ doSide left r, walled)
    | otherwise = (r,False)
    where
        overflow xs r' = if not (wallOnThe xs)
            then HM.insert (lastOnThe xs) (Just Flowing) r'
            else r'
        walled = wallOnThe right && wallOnThe left
        wallOnThe xs = isWall (lastOnThe xs)
        lastOnThe xs = head $ dropWhile (\c -> hasGround r c && not (isWall c)) xs
        isWall c = case HM.lookup c r of
            Just Nothing -> True
            _ -> False
        doSide xs r' = overflow xs 
            $ foldr (`HM.insert` (Just $ if walled then Stable else Flowing)) r' 
            $ takeWhile (\c -> hasGround r c && not (isWall c)) xs
        left = map (`V2` y) [(x-1),(x-2)..]
        right = map (`V2` y) [x,(x+1)..]

unsafeShowR :: R -> a -> a
unsafeShowR s expr = unsafePerformIO $ do
    printR s
    return expr

printR = traverse_ putStr . showR
showR :: R -> [String]
showR r = map (\y -> map (\x -> maybe '.' (maybe '#' (bool '|' '~' . (== Stable))) $ (V2 x y) `HM.lookup` r) [minX..maxX] ++ "\n") [0..lowestY r]
    where 
        k = HM.keys r
        (minX,maxX) = (view _x $ minimumBy (compare `on` view _x) k, view _x $ maximumBy (compare `on` view _x) k)  

test :: R
test = fromJust $ parseMaybe totP $ "x=495, y=2..7\ny=7, x=495..501\nx=501, y=3..7\nx=498, y=2..4\nx=506, y=1..2\nx=498, y=10..13\nx=504, y=10..13\ny=13, x=498..504"