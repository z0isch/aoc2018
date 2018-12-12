module Day9 where

import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Sequence   (Seq)
import qualified Data.Sequence   as S

type Scores = Map Int Integer

data S = S
    { sScores  :: !Scores
    , sCurr    :: !Int
    , sMarbles :: !(Seq Int)
    }
    deriving (Eq,Show)

initial :: Int -> S
initial numPlayers = S (M.fromList $ zip [1..numPlayers] (repeat 0)) 1 (S.fromList [0,1])

placeMarble :: S -> Int -> S
placeMarble (S scores curr ms) new
    | new `mod` 23 == 0 = S scores' removeMarbleIdx (S.deleteAt removeMarbleIdx ms)
    | otherwise = S scores idx' (S.insertAt idx' new ms)
    where
        pl = 1 + (new `mod` (M.size scores))
        removeMarbleIdx = (curr - 7) `mod` (length ms)
        scores' = M.adjust (+ fromIntegral (new + S.index ms removeMarbleIdx)) pl scores

        idx = (curr + 2) `mod` (length ms)
        idx' = if idx == 0 then length ms else idx

playGame' :: Int -> Int -> [S]
playGame' numPlayers marbles = scanl' placeMarble (initial numPlayers) [2..marbles]

playGame :: Int -> Int -> S
playGame numPlayers marbles = foldl' placeMarble (initial numPlayers) [2..marbles]

part1Sol :: Int -> Int -> Integer
part1Sol numPlayers marbles = maximum $ sScores $ playGame numPlayers marbles

part2Sol :: Int -> Int -> Integer
part2Sol numPlayers marbles = maximum $ sScores $ playGame numPlayers marbles

part1 :: Integer
part1 = part1Sol 423 71944

part2 :: Integer
part2 = part2Sol 423 (71944 * 100)

testWorks :: [Bool]
testWorks = zipWith testEq (lines test) (playGame' 9 25)

testEq :: String -> S -> Bool
testEq t (S _ p m) = parseLine t == (p,m)

parseLine :: String -> (Int,Seq Int)
parseLine xs = (length $ takeWhile ((/=) '(' . head ) ws, S.fromList $ map read ws)
    where ws = words xs

test :: String
test = "0 (1)\n0 (2) 1 \n0  2  1 (3)\n0 (4) 2  1  3 \n0  4  2 (5) 1  3 \n0  4  2  5  1 (6) 3 \n0  4  2  5  1  6  3 (7)\n0 (8) 4  2  5  1  6  3  7 \n0  8  4 (9) 2  5  1  6  3  7 \n0  8  4  9  2 (10) 5  1  6  3  7 \n0  8  4  9  2 10  5 (11) 1  6  3  7 \n0  8  4  9  2 10  5 11  1 (12) 6  3  7 \n0  8  4  9  2 10  5 11  1 12  6 (13) 3  7 \n0  8  4  9  2 10  5 11  1 12  6 13  3 (14) 7 \n0  8  4  9  2 10  5 11  1 12  6 13  3 14  7 (15)\n0 (16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15 \n0 16  8 (17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 (18) 9  2 10  5 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18  9 (19) 2 10  5 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18  9 19  2 (20) 10  5 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18  9 19  2 20 10 (21) 5 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18  9 19  2 20 10 21  5 (22) 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18 (19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18 19  2 (24) 20 10 21  5 22 11  1 12  6 13  3 14  7 15 \n0 16  8 17  4 18 19  2 24 20 (25) 10 21  5 22 11  1 12  6 13  3 14  7 15"
