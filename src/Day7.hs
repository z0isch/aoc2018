{-# LANGUAGE LambdaCase #-}

module Day7 where

import           Data.Char
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Set        (Set)
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

type Step = Char
type Req = (Step,Step)
type Deps = Map Step [Step]
input :: IO [Req]
input = parseInput . T.unpack <$> TIO.readFile "./input/day7.txt"

parseInput :: String -> [Req]
parseInput ('S':'t':'e':'p':' ':x:' ':'m':'u':'s':'t':' ':'b':'e':' ':'f':'i':'n':'i':'s':'h':'e':'d':' ':'b':'e':'f':'o':'r':'e':' ':'s':'t':'e':'p':' ':y:' ':'c':'a':'n':' ':'b':'e':'g':'i':'n':'.':xs) = (x,y):if null xs then [] else parseInput (tail xs)
parseInput _ = error "Bad input"

allSteps :: [Req] -> Set Step
allSteps cs = S.fromList $ map fst cs ++ map snd cs

reqs :: [Req] -> Map Step [Step]
reqs rs = M.fromListWith ((++)) $ concatMap (\(x,y) -> [(y,[x])]) rs

nextSteps :: Set Step -> Deps -> Set Step
nextSteps s m = S.filter (`M.notMember` m) s

nextStep :: Set Step -> Deps -> Step
nextStep s m = S.findMin $ S.filter (`M.notMember` m) s

doStep :: Deps -> Step -> Deps
doStep m s = M.filter (not . null) $ fmap (delete s) m

steps :: [Req] -> [Step]
steps cs = go aSteps mempty (reqs cs) []
  where
    aSteps = allSteps cs
    go unDone done rs order
      | S.size aSteps == S.size done = order
      | otherwise = go (S.delete step unDone) (S.insert step done) (doStep rs step) (order++[step])
        where step = nextStep unDone rs

type Time = Int
type WorkerId = Int
type WorkLoad = (Maybe (Step, Time))
type Workers = Map WorkerId WorkLoad

tickStep :: Int -> Map Step [Step] -> Workers -> (Map Step [Step], Workers, Set Step)
tickStep offset m ws = (m',ws',done)
    where
      m' = foldl' doStep m done
      ws' = fmap tickWorker ws
      tickWorker Nothing = Nothing
      tickWorker (Just (c,t))
        | t == (ord c - 64) + offset = Nothing
        | otherwise = Just (c,t+1)
      done = S.fromList $ catMaybes $ map (fmap fst) $ M.elems $ M.filter doneFilter ws
      doneFilter Nothing = False
      doneFilter (Just (c,t))
        | t == (ord c - 64) + offset = True
        | otherwise = False

data CState = CState
  { cNumWorkers   :: Int
  , cOffset       :: Int
  , cNotWorkingOn :: Set Step
  , cDone         :: Set Step
  , cWorkers      :: Workers
  , cDeps         :: Deps
  , cTime         :: Int
  }
  deriving (Eq,Show)

foo numWorkers offset = iterate goStep . initState numWorkers offset

part2Sol numWorkers offset rs = head $ dropWhile ((< S.size (allSteps rs)) . S.size . cDone) $ foo numWorkers offset rs

initState numWorkers offset cs = CState numWorkers offset aSteps mempty boredWorkers (reqs cs) (-1)
  where
    aSteps = allSteps cs
    boredWorkers = M.fromList $ map (\x -> (x,Nothing)) [1..numWorkers]

goStep :: CState -> CState
goStep (CState numWorkers offset notWorkingOn done ws rs t) = CState numWorkers offset notWorkingOn' done' ws' rs' (t+1)
    where

      justFinishedWorkers = M.filter justDoneFilter ws
      justDoneFilter Nothing = False
      justDoneFilter (Just (c,t))
        | t == (ord c - 64) + offset = True
        | otherwise = False

      justNowDone = S.fromList $ catMaybes $ map (fmap fst) $ M.elems $ justFinishedWorkers
      done' = S.union done justNowDone
      rs' = foldl' doStep rs justNowDone

      availableWorkers = M.filter available ws
      available Nothing = True
      available (Just (c,t))
        | t == (ord c - 64) + offset = True
        | otherwise = False

      stepCandidates = nextSteps notWorkingOn rs'
      stepsAbleToDo = zip (M.keys availableWorkers) (S.toList stepCandidates)
      assignedWorkers = foldr (\(wId,step) -> M.adjust (const $ Just (step,0)) wId) ws stepsAbleToDo
      notWorkingOn' = foldr S.delete notWorkingOn $ map snd stepsAbleToDo

      ws' = fmap tickWorker assignedWorkers
      tickWorker Nothing = Nothing
      tickWorker (Just (c,wt))
        | wt == (ord c - 64) + offset = Nothing
        | otherwise = Just (c,wt+1)

part1 :: IO [Step]
part1= steps <$> input

part2 :: IO Int
part2= cTime . part2Sol 5 60 <$> input

test :: [Req]
test= parseInput "Step C must be finished before step A can begin.\nStep C must be finished before step F can begin.\nStep A must be finished before step B can begin.\nStep A must be finished before step D can begin.\nStep B must be finished before step E can begin.\nStep D must be finished before step E can begin.\nStep F must be finished before step E can begin."
