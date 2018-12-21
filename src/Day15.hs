{-# LANGUAGE TemplateHaskell #-}

module Day15 where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State
import           Control.Parallel.Strategies
import           Data.Array
import           Data.Array                  as A
import           Data.Array                  (Array)
import           Data.Function
import qualified Data.Graph.Inductive.Graph  as G
import           Data.List
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as Seq
import           Data.Set                    (Set)
import qualified Data.Set                    as S
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Debug.Trace
import           Safe                        hiding (at)
import           System.IO.Unsafe

data C = C { _cX, _cY :: !Int}
    deriving (Eq,Show,Ord)
makeLenses ''C

type Hp = Int
type Ap = Int

data Stats = Stats
    { _sAp :: !Ap
    , _sHp :: !Hp
    }
    deriving (Eq,Show)
makeLenses ''Stats

data UnitType = G | E
    deriving (Eq,Show)
makePrisms ''UnitType

data Unit = Unit
    { _uType  :: !UnitType
    , _uStats :: !Stats
    }
    deriving (Eq,Show)
makeLenses ''Unit

data Tile = W | T (Maybe Unit)
    deriving (Eq,Show)
makePrisms ''Tile

type Cave = Map C Tile
type Path = Seq C

data S = S
    { _sCave  :: !Cave
    , _sRound :: !Integer
    }
    deriving (Eq,Show)
makeLenses ''S

input :: IO Cave
input = caveP . lines . T.unpack <$> TIO.readFile "./input/day15.txt"

input' fn = caveP . lines . T.unpack <$> TIO.readFile fn

tileP :: Char -> Tile
tileP '.' = T Nothing
tileP 'E' = T $ Just $ Unit E $ Stats 3 200
tileP 'G' = T $ Just $ Unit G $ Stats 3 200
tileP '#' = W

caveP :: [String] -> Map C Tile
caveP cs = M.fromList $ concat $ zipWith (\y -> map $ \(x,t) -> (C x y,t)) [0..length cs] $ map (zip [0..length (head cs)] . map tileP) cs

readingCompare :: C -> C -> Ordering
readingCompare (C x1 y1) (C x2 y2)
    | y1 == y2 = compare x1 x2
    | otherwise = compare y1 y2

hasUnit :: Tile -> Bool
hasUnit (T (Just _)) = True
hasUnit _            = False

isEnemy :: Unit -> Unit -> Bool
isEnemy (Unit E _) (Unit G _) = True
isEnemy (Unit G _) (Unit E _) = True
isEnemy _ _                   = False

enemies :: Cave -> C -> [C]
enemies cave c = case cave M.! c of
        (T (Just u)) -> M.keys $ M.filter (maybe False (isEnemy u) . preview (_T._Just)) cave
        _ -> unsafeShowCave cave $ error $ "Can't find " ++ show c

isOpen :: Cave -> C -> Bool
isOpen cave c = Just Nothing == atC
    where atC = cave^?at c._Just._T

moveTargets :: Cave -> C -> Set C
moveTargets cave c = S.fromList $ filter (isOpen cave) $ concatMap adjacents $ enemies cave c

adjacents :: C -> [C]
adjacents (C x y) = [C (x+1) y, C (x-1) y, C x (y+1), C x (y-1)]

manhattenDist :: C -> C -> Int
manhattenDist (C x1 y1) (C x2 y2) = abs(y2 - y1) + abs(x2 - x1)

newtype Sp = Sp (Seq C) deriving (Eq, Show)
instance Ord Sp where
    compare (Sp s1) (Sp s2) = case (compare (Seq.length s1) (Seq.length s2)) of
        EQ -> GT
        c  -> c

unsafeShowCave :: Cave -> a -> a
unsafeShowCave s expr = unsafePerformIO $ do
    showCave s
    return expr

sps :: Cave -> C -> Maybe (Seq Path)
sps cave c = if null candidates then Nothing else Just candidates
    where
        candidates = go (S.singleton c) maxBound (S.singleton (Sp $ pure c)) mempty
        go :: Set C -> Int -> Set Sp -> Seq Path -> Seq Path
        go seen spLength ps found
            | S.null ps = found
            | spLength < Seq.length path = found
            | nextToEnemy = go seen' (Seq.length path) ps' (found Seq.|> path)
            | otherwise = go seen' spLength ps'' found
            where
                seen' = S.insert currP seen
                (_ Seq.:<| path) = totP
                (Sp totP@(_ Seq.:|> currP), ps') = S.deleteFindMin ps
                nextToEnemy = any (`elem` myEnemies) as
                myEnemies = S.fromList $ enemies cave c
                as = adjacents currP
                next = map (\c' -> Sp (totP Seq.|> c'))
                    $ filter (\c' -> isOpen cave c' && c' `S.notMember` seen)
                    $ as
                ps'' = foldr S.insert ps' next

leastsBy :: (Eq b) => (b -> b -> Ordering) -> (a->b) -> [a] -> Maybe [a]
leastsBy f g = headMay . groupBy ((==) `on` g) . sortBy (f `on` g)

moveTargetPath :: Cave -> C -> Maybe Path
moveTargetPath cave c = minimumBy (readingCompare `on` (\(h Seq.:<| _) -> h)) <$> sps cave c

attackTarget :: Cave -> C -> Maybe C
attackTarget cave c = do
    lowestHps <- leastsBy compare (view (_2.uStats.sHp)) $ adjacentEnemies
    (a,_) <- headMay (sortBy (readingCompare `on` fst) lowestHps)
    pure a
    where
        adjacentEnemies = mapMaybe (\a -> (cave^?at a._Just._T._Just) >>= \u2 -> if isEnemy u u2 then pure (a,u2) else Nothing ) $ adjacents c
        (T (Just u)) = cave M.! c

unitTurn :: Monad m => C -> StateT S m Bool
unitTurn c = do
    s <- get
    if null $ enemies (s^.sCave) c
    then pure False
    else case attackTarget (s^.sCave) c of
        Nothing -> trace "finding path" $ case moveTargetPath (s^.sCave) c of
            Nothing -> trace "no path" $ pure True
            Just (mt Seq.:<| _) -> trace ("moving: " ++ show mt) $ do
                sCave.ix mt .= T (s^?sCave.at c._Just._T._Just)
                sCave.ix c .= T Nothing
                doAttack mt
        Just _ -> doAttack c

doAttack :: Monad m => C -> StateT S m Bool
doAttack c = do
    s <- get
    case attackTarget (s^.sCave) c of
        Nothing -> trace "not attacking" $ pure True
        Just at -> trace ("attacking: " ++ show at) $ do
            [ap] <- use (sCave.ix c._T._Just.uStats.sAp.to pure)
            sCave.ix at._T._Just.uStats.sHp -= ap
            [hp] <- use (sCave.ix at._T._Just.uStats.sHp.to pure)
            when (hp <= 0) $ trace (show at ++ " has died") $ sCave.ix at._T .= Nothing
            pure True

doRound :: StateT S IO Bool
doRound = do
    s <- get
    finished <- foldM foo True
        $ sortBy readingCompare
        $ M.keys
        $ M.filter hasUnit
        $ s^.sCave
    when finished $ sRound += 1
    get >>= lift . showS
    pure finished
    where
        foo b c = do
            s <- get
            if b
            then if isNothing (s^?sCave.at c._Just._T._Just)
                then pure b
                else traceShow (c, s^?sCave.at c._Just._T._Just) $ unitTurn c
            else pure b

part1Sol :: StateT S IO (Integer, (Sum Int))
part1Sol = do
    _ <- iterateWhile id doRound
    s <- get
    let totHp = foldMap (Sum . fromMaybe 0 . (preview (_T._Just.uStats.sHp))) $ M.filter hasUnit $ s^.sCave
    pure $ (s^.sRound,totHp)

part1 = do
    c <- input
    (r,hp) <- evalStateT part1Sol (S c 0)
    pure $ fromIntegral r * hp

showTile :: Tile -> Char
showTile W                     = '#'
showTile (T Nothing)           = '.'
showTile (T (Just (Unit E _))) = 'E'
showTile (T (Just (Unit G _))) = 'G'

showS :: S -> IO ()
showS s = showCave (s^.sCave) >> print (show (M.filter hasUnit (s^.sCave)))

showCave :: Cave -> IO ()
showCave c = mapM_ (putStrLn.foldMap (pure.showTile.snd)) $ Seq.chunksOf (maxX + 1) $ Seq.fromList $ sortBy (readingCompare `on` fst) $ M.toList c
    where
        maxX = maximum $ map _cX $ M.keys c

testMoveTargets :: Bool
testMoveTargets = expected == actual
    where
        actual = sortBy readingCompare $ S.toList $ moveTargets test2 (C 1 1)
        expected = [C 3 1, C 5 1, C 2 2,C 5 2, C 1 3, C 3 3 ]

test7 :: Cave
test7 = caveP $ lines "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######"

test6 :: Cave
test6 = caveP $ lines "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########"

test5 :: Cave
test5= caveP $ lines "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######"

test4 :: Cave
test4 = caveP $ lines "#########\n#G..G..G#\n#.......#\n#.......#\n#G..E..G#\n#.......#\n#.......#\n#G..G..G#\n#########"

test3 :: Cave
test3 = caveP $ lines "#######\n#.E...#\n#.....#\n#...G.#\n#######"

test2 :: Cave
test2 = caveP $ lines "#######\n#E..G.#\n#...#.#\n#.G.#G#\n#######"

test1 :: Cave
test1 = caveP $ lines $ "#######\n#.G.E.#\n#E.G.E#\n#.G.E.#\n#######"

testReadingOrder :: Bool
testReadingOrder = expected == actual
    where
        actual =  sortBy readingCompare $ M.keys $ M.filter hasUnit $ test1
        expected = [C 2 1, C 4 1, C 1 2, C 3 2, C 5 2, C 2 3, C 4 3]
