{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Day22 where

import           Control.Lens
import           Control.Monad.State.Strict
import           Control.Parallel.Strategies
import           Data.Foldable
import           Data.Function
import           Data.Hashable
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HM
import           Data.HashPSQ                (HashPSQ)
import qualified Data.HashPSQ                as PSQ
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as HS
import           Data.Ix
import           Data.List
import           Data.Maybe
import qualified Data.MemoCombinators        as Memo
import           Debug.Trace
import           GHC.Generics                (Generic)
import           Linear.V2

type Depth = Integer
type C = V2 Integer

data Equipment = Torch | Gear | Neither deriving (Eq,Show,Ord, Generic)
instance Hashable Equipment

data Action = Move C | Equip Equipment deriving (Eq,Show)
data S = S
    { _sC            :: !C
    , _sEq           :: !Equipment
    , _sTime         :: !Integer
    , _sToolSwitches :: !Integer
    , _sSteps        :: !Integer
    } deriving (Eq,Show, Ord, Generic)
instance Hashable S
makeLenses ''S

regionType :: (C,Depth) -> C -> Integer
regionType t c = (erosionLevel t c) `mod` 3

erosionLevel :: (C, Depth) -> C -> Integer
erosionLevel t@(target@(V2 tx ty),d) = Memo.unsafeArrayRange (V2 0 0, V2 (tx+75) (ty+75)) erosionLevel'
    where
        erosionLevel' c@(V2 x y)
            | target == c || (x == 0 && y == 0) = 0
            | y == 0 = (x * 16807 + d) `mod` 20183
            | x == 0 = (y * 48271 + d) `mod` 20183
            | otherwise = (erosionLevel t (V2 (x-1) y) * erosionLevel t (V2 x (y-1)) + d) `mod` 20183

part1Sol (target,d) = sum $ parMap rpar (regionType (target,d)) $ range (V2 0 0,target)

part1 = part1Sol (V2 6 797,11991)

manhattenDist (V2 x1 y1) (V2 x2 y2) = abs(x1-x2) + abs (y1-y2)

regionCache :: (C,Integer) -> HashMap C Integer
regionCache t@((V2 tx ty),_) = HM.fromList $ parMap rpar (\c -> (c,regionType t c)) $ range (V2 0 0, V2 (tx + 75) (ty + 75))

eqAllowed :: Integer -> [Equipment]
eqAllowed 0 = [Gear, Torch]
eqAllowed 1 = [Gear, Neither]
eqAllowed 2 = [Torch, Neither]

doAction :: Action -> State S ()
doAction (Move c) = do
    sC .= c
    sTime += 1
    sSteps += 1
doAction (Equip e) = do
    sEq .= e
    sTime += 7
    sToolSwitches += 1

nextS :: (C, Depth) -> HashMap (C,Equipment) Integer -> S -> HashMap C Integer -> [S]
nextS (V2 tx ty,_) seen s regions = filter (\s' -> maybe True (s'^.sTime <) $ HM.lookup (s'^.sC,s'^.sEq) seen)
    $ map ((`execState` s) . doAction)
    $ moveActions ++ eqActions
    where
        moveCs = filter (\(V2 x y) -> x >=0 && y >= 0 && x <= tx + 75 && y <= ty + 75)
            $ map ((s^.sC +)) [V2 1 0, V2 0 1, V2 0 (-1), V2 (-1) 0]
        lookupR = (regions HM.!)
        moveActions = map Move
            $ filter ((s^.sEq `elem`) . eqAllowed . lookupR)
            moveCs
        eqActions = map Equip
            $ delete (s^.sEq)
            $ eqAllowed $ lookupR (s^.sC)

bfs :: (C,Depth) -> Maybe S
bfs t@(target,_) = step (regionCache t) mempty (psqInsert (S 0 Torch 0 0 0) PSQ.empty) Nothing
    where
        psqInsert s psq = PSQ.insert s (s^.sTime) s psq
        step :: HashMap C Integer -> HashMap (C,Equipment) Integer -> HashPSQ S Integer S -> Maybe S -> Maybe S
        step !regions seen psq minS = case PSQ.minView psq of
            Nothing           -> Nothing
            Just (_,_,s,psq') ->
                if outOfTime s
                then minS
                else if target == (s^.sC)
                    then traceShow s $ if Torch == (s^.sEq)
                        then step regions seen psq' (if betterS s then Just s else minS)
                        else let s' = execState (doAction (Equip Torch)) s
                            in step regions seen' psq' (if betterS s' then Just s' else minS)
                    else step regions seen' psq'' minS
                where
                    betterS s' = maybe True (((s'^.sTime) < ) . view sTime) minS
                    outOfTime s' = maybe False (((s'^.sTime) > ) . view sTime) minS
                    seen' = HM.insert (s^.sC, s^.sEq) (s^.sTime) seen
                    psq'' = foldr psqInsert psq' $ nextS t seen' s regions

part2Sol t = (bfs t)^?_Just.sTime
part2 = part2Sol (V2 6 797,11991)
part2Test= part2Sol (V2 10 10, 510)
