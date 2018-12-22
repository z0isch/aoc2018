{-# LANGUAGE TemplateHaskell #-}

module Day15 where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.State
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as HS
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq
import           Data.Set            (Set)
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Linear.V2
import           Safe                hiding (at)
import           System.IO.Unsafe

type C = V2 Int
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
    , _uId    :: !Int
    }
    deriving (Eq,Show)
makeLenses ''Unit

data Tile = W | T (Maybe Unit)
    deriving (Eq,Show)
makePrisms ''Tile

type Cave = HashMap C Tile
type Path = Seq C

data S = S
    { _sCave  :: !Cave
    , _sRound :: !Integer
    , _sMoved :: !(HashSet Int)
    }
    deriving (Eq,Show)
makeLenses ''S

newtype Sp = Sp (Seq C) deriving (Eq, Show)
instance Ord Sp where
    compare (Sp s1) (Sp s2) = case (compare (Seq.length s1) (Seq.length s2)) of
        EQ -> GT
        c  -> c

tileP :: Ap -> Int -> Char -> Tile
tileP _ _ '.'  = T Nothing
tileP ap i 'E' = T $ Just $ Unit E (Stats ap 200) i
tileP _ i 'G'  = T $ Just $ Unit G (Stats 3 200) i
tileP _ _ '#'  = W

caveP :: Ap -> [String] -> Cave
caveP ap = M.fromList
    . concatMap (\(y,cs) -> zipWith (\x c -> (V2 x y, tileP ap (x*y) c)) [0..] cs)
    . zip [0..]

readingCompare :: C -> C -> Ordering
readingCompare (V2 x1 y1) (V2 x2 y2)
    | y1 == y2 = compare x1 x2
    | otherwise = compare y1 y2

hasUnit :: Tile -> Bool
hasUnit (T (Just _)) = True
hasUnit _            = False

isEnemy :: Unit -> Unit -> Bool
isEnemy (Unit E _ _) (Unit G _ _) = True
isEnemy (Unit G _ _) (Unit E _ _) = True
isEnemy _ _                       = False

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
adjacents (V2 x y) = [V2 (x+1) y, V2 (x-1) y, V2 x (y+1), V2 x (y-1)]

sps :: Cave -> C -> Maybe (HashSet (C,C))
sps cave c = if null candidates then Nothing else Just candidates
    where
        candidates = go (HS.singleton c) maxBound (S.singleton (0,Nothing,c)) mempty
        go :: HashSet C -> Int -> (Set (Int, Maybe C, C)) -> HashSet (C,C) -> HashSet (C,C)
        go seen spLength ps found
            | S.null ps = found
            | spLength < l = found
            | nextToEnemy = go seen' l ps' (HS.insert (fromJust h,currP) found)
            | otherwise = go seen' spLength ps'' found
            where
                ((l,h,currP), ps') = S.deleteFindMin ps
                as = adjacents currP
                seen' = HS.insert currP seen
                nextToEnemy = any (`elem` myEnemies) as
                myEnemies = enemies cave c
                next = map (\c' -> (l+1,maybe (Just c') Just h,c'))
                    $ filter (\c' -> isOpen cave c' && not (c' `HS.member` seen))
                    $ as
                ps'' = foldr S.insert ps' next

leastsBy :: (Eq b) => (b -> b -> Ordering) -> (a->b) -> [a] -> Maybe [a]
leastsBy f g = headMay . groupBy ((==) `on` g) . sortBy (f `on` g)

moveTargetPath :: Cave -> C -> Maybe C
moveTargetPath cave c = do
    rs <- leastsBy readingCompare snd . HS.toList <$> sps cave c
    fst . minimumBy (readingCompare `on` fst) <$> rs

attackTarget :: Cave -> C -> Maybe C
attackTarget cave c = do
    lowestHps <- leastsBy compare (view (_2.uStats.sHp)) adjacentEnemies
    (a,_) <- headMay $ sortBy (readingCompare `on` fst) lowestHps
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
        Nothing -> case moveTargetPath (s^.sCave) c of
            Nothing -> pure True
            Just mt -> do
                sCave.ix mt .= T (s^?sCave.ix c._T._Just)
                sCave.ix c .= T Nothing
                doAttack mt
        Just _ -> doAttack c

doAttack :: Monad m => C -> StateT S m Bool
doAttack c = do
    s <- get
    case attackTarget (s^.sCave) c of
        Nothing -> pure True
        Just at -> do
            [ap] <- use (sCave.ix c._T._Just.uStats.sAp.to pure)
            sCave.ix at._T._Just.uStats.sHp -= ap
            [hp] <- use (sCave.ix at._T._Just.uStats.sHp.to pure)
            when (hp <= 0) $ sCave.ix at._T .= Nothing
            pure True

doRound :: State S Bool
doRound = do
    s <- get
    sMoved .= mempty
    finished <- foldM foo True
        $ sortBy readingCompare
        $ M.keys
        $ M.filter hasUnit
        $ s^.sCave
    when finished $ sRound += 1
    pure finished
    where
        foo b c = do
            s <- get
            let me = s^?sCave.ix c._T._Just.uId
            if b
            then if isNothing me || s^.sMoved.contains (fromJust me)
                then pure b
                else do
                    sMoved %= HS.insert (fromJust me)
                    unitTurn c
            else pure b

sol :: Ap -> [String] -> (Int, S)
sol x i = (eL,s)
    where
        initial = (S (caveP x i) 0 mempty)
        eL = elfLength initial
        s = execState (iterateWhile id $ doRound) initial

totHp :: S -> Sum Hp
totHp s = foldMap (Sum . fromMaybe 0 . (preview (_T._Just.uStats.sHp))) $ M.filter hasUnit $ s^.sCave

elfLength :: S -> Int
elfLength s = length $ M.filter (has (_T._Just.uType._E)) $ s^.sCave

part1 = do
    i <- lines . T.unpack <$> TIO.readFile "./input/day15.txt"
    let (_,s) = sol 3 i
    pure $ ((s^.sRound),(totHp s))

part2Sol i = ((s^.sRound),(totHp s))
    where
        (_,s) = head $ dropWhile (\(eL,s) -> elfLength s < eL) $ zipWith sol [4..] (repeat i)

part2 = part2Sol . lines . T.unpack <$> TIO.readFile "./input/day15.txt"

unsafeShowCave :: Cave -> a -> a
unsafeShowCave s expr = unsafePerformIO $ do
    showCave s
    return expr

showTile :: Tile -> Char
showTile W                       = '#'
showTile (T Nothing)             = '.'
showTile (T (Just (Unit E _ _))) = 'E'
showTile (T (Just (Unit G _ _))) = 'G'

showS :: S -> IO ()
showS s = showCave (s^.sCave) >> print (show (M.filter hasUnit (s^.sCave)))

showCave :: Cave -> IO ()
showCave c = mapM_ (putStrLn.foldMap (pure.showTile.snd)) $ Seq.chunksOf (maxX + 1) $ Seq.fromList $ sortBy (readingCompare `on` fst) $ M.toList c
    where
        maxX = maximum $ map (view _x) $ M.keys c

test7 :: [String]
test7 = lines "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######"

test6 :: [String]
test6 = lines "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########"

test5 :: [String]
test5= lines "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######"

test4 :: Cave
test4 = caveP 3 $ lines "#########\n#G..G..G#\n#.......#\n#.......#\n#G..E..G#\n#.......#\n#.......#\n#G..G..G#\n#########"

test3 :: Cave
test3 = caveP 3 $ lines "#######\n#.E...#\n#.....#\n#...G.#\n#######"

test2 :: Cave
test2 = caveP 3 $ lines "#######\n#E..G.#\n#...#.#\n#.G.#G#\n#######"

test1 :: Cave
test1 = caveP 3 $ lines $ "#######\n#.G.E.#\n#E.G.E#\n#.G.E.#\n#######"
