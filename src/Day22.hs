
module Day22 where

import Control.Parallel.Strategies
import Data.Function (fix)
import Linear.V2
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import qualified Data.MemoCombinators as Memo

type Depth = Integer
type C = V2 Integer

regionType :: C -> (C,Depth) -> C -> Integer
regionType size (target,d) c = (erosionLevelFast size (target,d) c) `mod` 3

erosionLevel :: (C,Depth) -> C -> Integer
erosionLevel (target,d) c = (geoIndex (target,d)  c + d) `mod` 20183

geoIndex :: (C,Depth) -> C -> Integer
geoIndex (target,d) c@(V2 x y)
    | target == c || (x == 0 && y == 0) = 0
    | y == 0 = x * 16807
    | x == 0 = y * 48271
    | otherwise = erosionLevel (target,d) (V2 (x-1) y) * erosionLevel (target,d) (V2 x (y-1))

part1Naive (target,d) (V2 x y) = sum $ concat $ parMap rpar (\x -> parMap rpar (\y -> regionType (V2 x y) (target,d) (V2 x y)) [0..y]) [0..x]

part1 = part1Naive (V2 6 797,11991) (V2 6 797)

test = regionType (V2 10 10) (V2 10 10, 510) (V2 1 1)

erosionLevelFast :: C -> (C, Depth) -> C -> Integer
erosionLevelFast size t = Memo.unsafeArrayRange (0,size) (erosionLevel t)