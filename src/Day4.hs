{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Maybe
import qualified Data.SegmentTree           as ST
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

data DateTime = DateTime
  { dtY   :: Integer
  , dtM   :: Integer
  , dtD   :: Integer
  , dtH   :: Integer
  , dtMin :: Integer
  }
  deriving (Show, Eq)
instance Ord DateTime where
  compare (DateTime y1 m1 d1 h1 min1) (DateTime y2 m2 d2 h2 min2)
    | y1 > y2 = GT
    | y1 < y2 = LT
    | m1 > m2 = GT
    | m1 < m2 = LT
    | d1 > d2 = GT
    | d1 < d2 = LT
    | h1 > h2 = GT
    | h1 < h2 = LT
    | min1 > min2 = GT
    | min1 < min2 = LT
    | otherwise = EQ

data Action
  = BeginShift Integer
  | Sleep
  | WakeUp
  deriving (Eq, Show)

parseInput :: IO [(DateTime, Action)]
parseInput = parse fileP "" <$> TIO.readFile "./input/day4.txt" >>= \case
    Left err -> error (parseErrorPretty err)
    Right x -> pure x

fileP :: Parsec Void Text [(DateTime, Action)]
fileP = many (totP <* (void eol <|> eof))

dtP :: Parsec Void Text DateTime
dtP = DateTime <$> decimal <* char '-' <*> decimal <* char '-' <*> decimal <* char ' ' <*> decimal <* char ':' <*> decimal

actionP :: Parsec Void Text Action
actionP = beg <|> sleep <|> wakeUp
  where
    beg = BeginShift <$> (string "Guard #" *> decimal <* " begins shift")
    sleep = Sleep <$ string "falls asleep"
    wakeUp = WakeUp <$ string "wakes up"

totP :: Parsec Void Text (DateTime,Action)
totP = (,) <$> between (char '[') (char ']') dtP <* char ' ' <*> actionP

type SleepInterval = (Integer,Integer)

mkSleepIntervals :: [(DateTime,Action)] -> Map Integer [SleepInterval]
mkSleepIntervals = go M.empty . sortOn fst
    where
      isBeginShift (BeginShift _) = True
      isBeginShift _              = False
      go m [] = m
      go m ((_,BeginShift i):xs) = go m' rest
        where
          currShift = takeWhile (not . isBeginShift . snd) xs
          rest = dropWhile (not . isBeginShift . snd) xs
          sched = zipWith mkSched currShift (tail currShift)
          m' = foldl' b m $ catMaybes sched
            where
              b x (k,v) = M.alter (Just . maybe [v] (\vs -> vs <> [v])) k x
          mkSched (d1,Sleep) (d2,_) = Just (i,(dtMin d1, dtMin d2 - 1))
          mkSched _ _               = Nothing
      go _ _ = error "Bad schedule"

maxGuardTimeAsleep :: Map Integer [SleepInterval] -> (Integer, Integer)
maxGuardTimeAsleep = maximumBy (compare `on` snd) . M.toList . fmap (sum . map (\(x,y) -> y-x))

maxMinute :: [SleepInterval] -> (Integer, Integer)
maxMinute rs = (maximumBy (compare `on` snd) $ map (\x -> (x, ST.countingQuery (ST.fromList rs) x)) [0..59])

maxGuardMinuteAsleep :: Map Integer [SleepInterval] -> (Integer, (Integer, Integer))
maxGuardMinuteAsleep = maximumBy (compare `on` (snd . snd)) . M.toList . fmap maxMinute

part1Sol :: [(DateTime,Action)] -> Integer
part1Sol xs = g * m
  where
    intervals = mkSleepIntervals xs
    (g,_) = maxGuardTimeAsleep intervals
    (m,_) = maxMinute (intervals M.! g)

part2Sol :: [(DateTime,Action)] -> Integer
part2Sol xs = g * m
  where
    (g,(m,_)) = maxGuardMinuteAsleep $ mkSleepIntervals xs

part1 :: IO Integer
part1 = part1Sol <$> parseInput

part2 :: IO Integer
part2 = part2Sol <$> parseInput

test :: Maybe [(DateTime,Action)]
test = parseMaybe fileP "[1518-11-01 00:00] Guard #10 begins shift\n[1518-11-01 00:05] falls asleep\n[1518-11-01 00:25] wakes up\n[1518-11-01 00:30] falls asleep\n[1518-11-01 00:55] wakes up\n[1518-11-01 23:58] Guard #99 begins shift\n[1518-11-02 00:40] falls asleep\n[1518-11-02 00:50] wakes up\n[1518-11-03 00:05] Guard #10 begins shift\n[1518-11-03 00:24] falls asleep\n[1518-11-03 00:29] wakes up\n[1518-11-04 00:02] Guard #99 begins shift\n[1518-11-04 00:36] falls asleep\n[1518-11-04 00:46] wakes up\n[1518-11-05 00:03] Guard #99 begins shift\n[1518-11-05 00:45] falls asleep\n[1518-11-05 00:55] wakes up"
