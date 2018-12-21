{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day20 where

import Data.Void
import           Data.Text                  (Text)
import   qualified        Data.Text  as T
import qualified Data.Text.IO               as TIO
import Data.Functor
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import Control.Monad.State.Strict
import Control.Lens
import Data.Maybe


data C = C {_cX, _cY :: !Int} deriving (Eq,Show,Ord)
makeLenses ''C

data S = S
    { _sDoors :: !(Map C [C])
    , _sPos :: !C
    , _sPosStack :: ![C]
    , _sStr :: !String
    }
    deriving (Eq,Show,Ord)
makeLenses ''S


initialS = S mempty (C 0 0) mempty

goDir :: Char -> C -> C
goDir = \case 
    'N' -> cY +~ 1
    'S' -> cY -~ 1
    'E' -> cX +~ 1
    'W' -> cX -~ 1

sP :: State S ()
sP = use sStr >>= \(x:xs) -> case x of
    '(' -> do
        c <- use sPos
        sPosStack %= (c:)
        sStr .= xs
        sP
    ')' -> do
        sPosStack %= tail
        sStr .= xs
        sP
    '|' -> do
        (p:_) <- use sPosStack
        sPos .= p
        sStr .= xs
        sP
    '$' -> pure ()
    '^' -> do
        sStr .= xs
        sP
    _ -> do
        c <- use sPos
        let n = goDir x c
        sDoors.at c %= maybe (Just $ pure n) (Just . (n:))
        sPos .= n
        sStr .= xs
        sP

totP :: String -> Map C [C]
totP i = execState sP (initialS i) ^. sDoors

search :: Map C [C] -> Map C Int
search graph = go [(C 0 0,0)] mempty
    where 
        go [] vs = vs
        go ((c,len):xs) vs =
            let next = map (\c' -> (c',len+1)) $ filter (`M.notMember` vs) $ fromMaybe [] $ M.lookup c graph
            in go (next ++ xs) (M.insert c len vs)

part1Sol = maximum . search . totP
part2Sol = length . M.filter (>= 1000) . search . totP

part1 = part1Sol . T.unpack <$> TIO.readFile "./input/day20.txt"
part2 = part2Sol . T.unpack <$> TIO.readFile "./input/day20.txt"

test :: String
test = "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"