module Day13 where

import           Control.Monad
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO
import           Safe

data C = C {cX, cY :: !Int }
  deriving (Eq, Show, Ord)

data Dir = U | D | L | R
  deriving (Eq, Show, Enum, Ord)

data Piece = UD | LR | BendR | BendL | Intersection
  deriving (Eq, Show, Ord)

type Tick = Integer
type Track = Map C Piece

data CS = CS
  { csC                 :: !C
  , csDir               :: !Dir
  , csTick              :: !Tick
  , csIntersectionTurns :: ![Dir]
  }
  deriving (Eq, Show)

data S = S
  { sTrack :: !Track
  , sCarts :: !(Map Integer CS)
  , sTick  :: !Tick
  }
  deriving (Eq, Show)

goDir :: C -> Dir -> Maybe C
goDir (C x y) U
  | y> 0 = Just $ C x (y-1)
  | otherwise = Nothing
goDir (C x y) D = Just $ C x (y+1)
goDir (C x y) L
  | x> 0 = Just $ C (x-1) y
  | otherwise = Nothing
goDir (C x y) R = Just $ C (x+1) y

adjacents :: Track -> C -> [(Dir,(C,Piece))]
adjacents t c = catMaybes $ map (\(d,c') -> (\p' -> (d,(c',p'))) <$> M.lookup c' t) coords
  where
    coords = catMaybes $ map (\d -> (\c' -> (d,c')) <$> goDir c d) $ enumFrom U

crash :: Map Integer CS -> Maybe C
crash cs = headMay $ [c1 | c1 <- cartCs, c2 <- delete c1 cartCs, c1 == c2]
  where cartCs = M.elems $ fmap csC cs

turnCart :: Dir -> Integer -> Dir
turnCart facing numTurns = case numTurns `mod` 3 of
  0 -> case facing of
    U -> L
    D -> R
    L -> D
    R -> U
  1 -> case facing of
    U -> U
    D -> D
    L -> L
    R -> R
  2 -> case facing of
    U -> R
    D -> L
    L -> U
    R -> D

goCart :: Track -> CS -> CS
goCart t cs@(CS c d tick turns) = cs'
    where
      cs' = CS c' d' (tick + 1) turns'
      (c',piece) = maybe (error $ show cs) snd $ headMay $ filter ((==) d . fst) $ adjacents t c
      d' = case piece of
        BendR        -> case d of
          U -> R
          D -> L
          L -> D
          R -> U
        BendL        -> case d of
          U -> L
          D -> R
          L -> U
          R -> D
        Intersection -> turnCart d (genericLength turns)
        _            -> d
      turns' = case piece of
        Intersection -> turns ++ [d']
        _            -> turns


goS :: S -> Either C S
goS (S t cs tick) = s' <$> foldM stopOnCrash cs (sortOn (csC.snd) $ M.toList cs)
  where
    s' cs' = S t cs' (tick + 1)
    stopOnCrash cs' (i,_) = case crash cs'' of
      (Just crashC) -> Left crashC
      Nothing       -> Right cs''
      where
        cs'' = M.adjust (goCart t) i cs'

goS2 :: S -> Either C S
goS2 (S t cs tick) = s' <$> foldM removeAfterCrash cs (sortOn (csC.snd) $ M.toList cs)
  where
    s' cs' = S t cs' (tick + 1)
    removeAfterCrash cs' (i,_) = case crash cs'' of
      (Just crashC) -> let removedCrashed = foldr M.delete cs'' $ M.keys $ M.filter ((==) crashC . csC) cs''
                       in if M.size removedCrashed == 1
                        then Left $ csC $
                          let lastCar = head $ M.elems removedCrashed
                          in if csTick lastCar == tick then goCart t lastCar else lastCar
                        else Right removedCrashed
      Nothing       -> Right cs''
      where
        cs'' = M.adjust (goCart t) i cs'

part1Sol :: S -> C
part1Sol = go
  where
    go s = case goS s of
      Left c   -> c
      Right s' -> go s'

part2Sol :: S -> C
part2Sol = go
  where
    go s = case goS2 s of
      Left c   -> c
      Right s' -> go s'

initial :: Map C (Either Dir Piece) -> S
initial i = S track carts 0
  where
    carts = M.fromList $ zip [0..] $ map (\(c,Left d) -> initialCart c d) $ M.toList $ M.filter isLeft i
    initialCart c d = CS c d 0 []
    track = makeTrack i

makeTrack :: Map C (Either Dir Piece) -> Track
makeTrack m = M.map foo m
  where
    foo (Right p) = p
    foo (Left U)  = UD
    foo (Left D)  = UD
    foo (Left L)  = LR
    foo (Left R)  = LR

parseInput :: String -> Map C (Either Dir Piece)
parseInput = M.fromList . catMaybes . concat . zipWith (\y cs -> map (\(x,p) -> (\t -> (C x y,t)) <$> p) cs) [0..] . map (zip [0..] . map tP) . lines

tP :: Char -> Maybe (Either Dir Piece)
tP '/'  = Just $ Right BendR
tP '\\' = Just $ Right BendL
tP '|'  = Just $ Right UD
tP '-'  = Just $ Right LR
tP '+'  = Just $ Right Intersection
tP '>'  = Just $ Left R
tP '<'  = Just $ Left L
tP '^'  = Just $ Left U
tP 'v'  = Just $ Left D
tP ' '  = Nothing

input :: IO (Map C (Either Dir Piece))
input = parseInput . T.unpack <$> TIO.readFile "./input/day13.txt"

part1 :: IO C
part1 = do
  i <- input
  pure $ part1Sol $ initial i

part2 :: IO C
part2 = do
  i <- input
  pure $ part2Sol $ initial i

printIt = input >>= writeFile "out.txt" . testPrint

testPrint :: Map C (Either Dir Piece) -> String
testPrint m = concatMap ((\x -> x ++ "\n") . map snd) strs
  where
    strs = groupBy ((==) `on` (cY.fst)) $ sortOn (cY.fst) $ M.toList $ M.union newM $ M.fromList $ map (\c -> (c,' ')) $ [C x y | x <- [0..maxX], y<- [0..maxY]]
    maxX = maximum $ map cX $ M.keys m
    maxY = maximum $ map cY $ M.keys m
    newM = fmap strP t
    strP UD           = '|'
    strP LR           = '-'
    strP Intersection = '+'
    strP BendL        = '\\'
    strP BendR        = '/'
    t= makeTrack m

runTest :: C
runTest = part1Sol $ initial $ parseInput test
test :: String
test = "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   "

runTest2 :: C
runTest2 = part2Sol test2
test2 :: S
test2 = initial $ parseInput $ "/>-<\\  \n|   |  \n| /<+-\\\n| | | v\n\\>+</ |\n  |   ^\n  \\<->/"
