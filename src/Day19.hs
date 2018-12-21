{-# LANGUAGE TemplateHaskell #-}

module Day19 where

import Day16 (OpCode(..), OpType(..), Reg, Val, runCode)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import Control.Monad.State.Strict
import Control.Lens
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Maybe
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO

type ProgLine = (OpCode,OpType,Integer, Integer, Integer)

totP :: String -> (Reg, [ProgLine])
totP xs = (instP $ head ls, map opP $ tail ls)
    where ls = lines xs

instP :: String -> Reg
instP ('#':'i':'p':' ':x) = read x

opP :: String -> ProgLine
opP xs = (opC,opT,r1,r2,r3)
    where
        iP :: String -> (Integer,String)
        iP xs = (read $ takeWhile (/= ' ') xs, dropWhile (/= ' ') xs)
        (r1,r1s) = iP $ drop 1 $ dropWhile (/= ' ') xs
        (r2,r2s) = iP $ drop 1 r1s
        (r3,_) = iP $ drop 1 r2s
        (opC,opT) = opStr $ takeWhile (/= ' ') xs

opStr :: String -> (OpCode,OpType)
opStr "addi" = (Add, OpI)
opStr "addr" = (Add, OpR)
opStr "muli" = (Mul, OpI)
opStr "mulr" = (Mul, OpR)
opStr "bani" = (Ban, OpI)
opStr "banr" = (Ban, OpR)
opStr "bori" = (Bor, OpI)
opStr "borr" = (Bor, OpR)
opStr "seti" = (Set, OpI)
opStr "setr" = (Set, OpR)
opStr "gtir" = (Gtr, OpIR)
opStr "gtri" = (Gtr, OpI)
opStr "gtrr" = (Gtr, OpR)
opStr "eqir" = (Eqi, OpIR)
opStr "eqri" = (Eqi, OpI)
opStr "eqrr" = (Eqi, OpR)
    
data S = S 
    { _sIp :: Integer
    , _sR :: Map Reg Val
    , _sLines :: Seq ProgLine
    , _sIpr :: Reg
    }
    deriving (Eq,Show)
makeLenses ''S

runProgLine :: State S Bool
runProgLine = do
    s <- get
    let ipr = _sIpr s
    case s^?sLines.ix (fromIntegral $ _sIp s) of
        Nothing -> pure True
        (Just (oC,oT,r1,r2,r3)) -> do
            sR.ix ipr .= _sIp s
            sR %= (\m -> runCode m oC oT (r1,r2,r3))
            s' <- get
            sIp .= fromJust (s'^?sR.ix ipr)
            sIp += 1
            pure False

initialS :: (Reg,[ProgLine]) -> S
initialS (ip,ps)= S 0 (M.fromList [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0)]) (S.fromList ps) ip

initialS2 :: (Reg,[ProgLine]) -> S
initialS2 (ip,ps)= S 0 (M.fromList [(0,1),(1,0),(2,0),(3,0),(4,0),(5,0)]) (S.fromList ps) ip

runProgram :: State S ()
runProgram = do
    done <- runProgLine
    if done then pure () else runProgram

part1Sol i = (execState runProgram i)^?sR.ix 0

part1= part1Sol . initialS . totP . T.unpack  <$> TIO.readFile "./input/day19.txt"
part2= part1Sol . initialS2 . totP . T.unpack  <$> TIO.readFile "./input/day19.txt"

test = initialS $ totP $ "#ip 0\nseti 5 0 1\nseti 6 0 2\naddi 0 1 0\naddr 1 2 3\nsetr 1 0 0\nseti 8 0 4\nseti 9 0 5"
