module Day9 where

import Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Scores = Map Int [Int]
initial :: Int -> (Scores,Int,[Int])
initial numPlayers = (M.fromList $ zip [1..numPlayers] (repeat []),1,[0,1])

removeAt :: Int -> [a] -> [a]
removeAt idx ms = take idx ms ++ drop (idx + 1) ms

placeMarble :: (Scores, Int,[Int]) -> Int -> (Scores,Int,[Int])
placeMarble (scores, curr,ms) new
    | new `mod` 23 == 0 = (scores', removeMarbleIdx, ms'')
    | otherwise = (scores, idx',ms')
    where
        pl = 1 + (new `mod` (M.size scores))
        removeMarbleIdx = (curr - 7) `mod` (length ms)
        scores' = M.adjust ((++) [new, ms !! removeMarbleIdx]) pl scores
        ms'' = removeAt removeMarbleIdx ms

        idx = (curr + 2) `mod` (length ms)
        idx' = if idx == 0 then length ms else idx
        before = take idx' ms
        after = drop idx' ms
        ms' = before ++ new:after

playGame :: Int -> Int -> [(Scores, Int,[Int])]
playGame numPlayers marbles = scanl' placeMarble (initial numPlayers) [2..marbles]

part1Sol numPlayers marbles = maximum $ map sum $ M.elems $ (\(s,_,_) -> s) $ last $ playGame numPlayers marbles

part1 = part1Sol 423 71944
part2 = undefined

testWorks :: [Bool]
testWorks = zipWith testEq (lines test) (playGame 9 25)

testEq :: String -> (Scores,Int,[Int]) -> Bool
testEq t (_,p,m) = parseLine t == (p,m)

parseLine :: String -> (Int,[Int])
parseLine xs = (length $ takeWhile ((/=) '(' . head ) ws, map read ws)
    where ws = words xs

test :: String
test = "0 (1)\n0 (2) 1 \n0  2  1 (3)\n0 (4) 2  1  3 \n0  4  2 (5) 1  3 \n0  4  2  5  1 (6) 3 \n0  4  2  5  1  6  3 (7)\n0 (8) 4  2  5  1  6  3  7 \n0  8  4 (9) 2  5  1  6  3  7 \n0  8  4  9  2 (10) 5  1  6  3  7 \n0  8  4  9  2 10  5 (11) 1  6  3  7 \n0  8  4  9  2 10  5 11  1 (12) 6  3  7 \n0  8  4  9  2 10  5 11  1 12  6 (13) 3  7 \n0  8  4  9  2 10  5 11  1 12  6 13  3 (14) 7 \n0  8  4  9  2 10  5 11  1 12  6 13  3 14  7 (15)\n0 (16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15 \n0 16  8 (17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 (18) 9  2 10  5 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18  9 (19) 2 10  5 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18  9 19  2 (20) 10  5 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18  9 19  2 20 10 (21) 5 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18  9 19  2 20 10 21  5 (22) 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18 (19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18 19  2 (24) 20 10 21  5 22 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18 19  2 24 20 (25) 10 21  5 22 11  1 12  6 13  3 14  7 15"