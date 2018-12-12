{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Day11 where

import           Control.Parallel.Strategies
import           Data.Function
import           Data.List
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import qualified Data.Massiv.Array           as A
import           GHC.Generics                hiding (C)

type SerialNumber = Int

data C = C {cX,cY :: !Int}
  deriving(Eq,Show,Ord,Generic)
instance NFData C where

powerLevel :: SerialNumber -> C -> Int
powerLevel sn (C x y)
  | length pLevel < 3 = 0
  | otherwise = (read [pLevel !! (length pLevel - 3)]) - 5
  where
    rackId = x + 10
    pLevel = show $ (rackId * y + sn)*rackId

powerLevelM :: SerialNumber -> Int -> Map C Int
powerLevelM sn side = M.fromList $ map (\c -> (c,powerLevel sn c)) $ [C x y | x <- [1..side], y <- [1..side]]

byX :: Int -> [a] -> [[a]]
byX sq ls = map (take sq) $ take ((length ls)-(sq-1)) $ iterate tail ls

xByx :: Int -> Int -> [[C]]
xByx side sq = [ [C x y | x <- xs, y <- ys] | xs <- sqLength, ys <- sqLength]
    where sqLength = byX sq [1..side]

part1Sol :: Map C Int -> Int -> Int -> (C, Int)
part1Sol m side sq = maximumBy (compare `on` snd) $ parMap rdeepseq (\(x:xs) -> (x, sum $ map (m M.!) (x:xs))) $ xByx side sq

part2Sol :: Map C Int -> Int -> (Int,(C, Int))
part2Sol m side = maximumBy (compare `on` (snd.snd)) $ map (\sq -> (sq, part1Sol m side sq)) [1..side]

part1 :: (C, Int)
part1 = part1Sol (powerLevelM 7403 300) 300 3

part2 :: (Int,(C, Int))
part2 = part2Sol (powerLevelM 7403 300) 300

powerLevelA :: SerialNumber -> Int -> A.Array A.D A.Ix2 Int
powerLevelA sn side = A.makeArray A.Seq (side A.:. side) (\(i A.:. j) -> powerLevel sn (C i j))

xByX' :: Int -> A.Stencil A.Ix2 Int Int
xByX' sq = A.makeStencil (A.Fill (-1000000)) (sq A.:. sq) (1 A.:. 1) $ \g ->
  sum $ [g (x A.:. y)| x <- [0..sq-1], y <- [0..sq-1]]

part1Sol' :: A.Array A.D A.Ix2 Int -> Int -> IO (C,Int)
part1Sol' a sq
  | sq == 1 = maxA $ A.imap (\(x A.:. y) v -> (C x y, v)) a
  |otherwise = maxA $ A.imap (\(x A.:. y) v -> (C x y, v)) applyStencil
    where
      applyStencil :: A.Array A.U A.Ix2 Int
      applyStencil = A.compute $ A.mapStencil (xByX' sq) (A.computeAs A.U a)
      maxA = A.foldrP f start f start
      f (c1,v1) (c2,v2) = if v1 > v2 then (c1,v1) else (c2,v2)
      start = (C 0 0, minBound)

part2Sol' :: A.Array A.D A.Ix2 Int -> Int -> IO (Int,(C, Int))
part2Sol' a side = do
  ss <- mapM (\sq -> (\v -> (sq,v)) <$> part1Sol' a sq) [1..side]
  pure $ maximumBy (compare `on` (snd.snd)) ss

part1' :: IO (C,Int)
part1' = part1Sol' (powerLevelA 7403 300) 3

part2' :: IO (Int, (C,Int))
part2' = part2Sol' (powerLevelA 7403 300) 300
