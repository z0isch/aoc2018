{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Day16 where

import           Control.Monad
import           Data.Bits
import           Data.List
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Text.Megaparsec.Char.Lexer

type Reg = Integer
type Val = Integer
type S = Map Reg Val
type ProgramLine = (Integer,(Integer,Integer,Integer))
type Input = (S,ProgramLine, S)

parseInput :: IO [Input]
parseInput = parse totP "" <$> TIO.readFile "./input/day16-1.txt" >>= \case
    Left err -> error (parseErrorPretty err)
    Right x -> pure x

parseProg :: IO [ProgramLine]
parseProg = parse (progP `sepBy` eol) "" <$> TIO.readFile "./input/day16-2.txt" >>= \case
  Left err -> error (parseErrorPretty err)
  Right x -> pure x

totP :: Parsec Void Text [Input]
totP = many (sP <* (void eol <|> eof))

progP :: Parsec Void Text ProgramLine
progP = (\[o,i1,i2,i3] -> (o,(i1,i2,i3))) <$> decimal `sepBy` " "

sP :: Parsec Void Text Input
sP = (\b p a -> (M.fromList $ zip [0..] b,p,M.fromList $ zip [0..] a))
  <$ "Before: " <*> between "[" "]" (decimal `sepBy` ", ") <* eol
  <*> progP <* eol
  <* "After:  " <*> between "[" "]" (decimal `sepBy` ", ") <* optional eol

data OpType
  = OpR | OpI | OpIR
  deriving (Eq, Show, Enum, Ord)

data OpCode = Add | Mul | Ban | Bor | Set | Gtr | Eqi
  deriving (Eq, Show, Enum, Ord)

doOp :: OpCode -> Integer -> Integer -> Integer
doOp Add x y = x + y
doOp Mul x y = x * y
doOp Ban x y = x .&. y
doOp Bor x y = x .|. y
doOp Set x _ = x
doOp Gtr x y = if x > y then 1 else 0
doOp Eqi x y = if x == y then 1 else 0

getVals :: S -> OpType -> (Integer, Integer, Integer) -> (Integer, Integer, Integer)
getVals m t (r1,r2,r3)= case t of
  OpR  -> (m M.! r1, m M.! r2, r3)
  OpI  -> (m M.! r1, r2, r3)
  OpIR -> (r1, m M.! r2, r3)

runCode :: S -> OpCode -> OpType -> (Integer, Integer, Integer) -> S
runCode m c t vs@(r1,_,r3) = M.adjust (const $ doOp c v1 v2) r m
  where
    (v1,v2,r) = case c of
      Set -> case t of
        OpI -> (r1,undefined,r3)
        _   -> getVals m t vs
      _ -> getVals m t vs

allCodes :: [(OpCode, OpType)]
allCodes = do
  t <- enumFrom OpR
  o <- enumFrom Add
  guard $ case o of
    Gtr -> True
    Eqi -> True
    _ -> case t of
      OpIR -> False
      _    -> True
  pure (o,t)

possibleCodes :: Input -> (Integer,[(OpCode,OpType)])
possibleCodes (inRegs,(opNum,input),outRegs) = (opNum, filter (\(o,t) -> runCode inRegs o t input == outRegs) allCodes)

getCodes :: [(Integer,[(OpCode,OpType)])] -> Map Integer (OpCode,OpType)
getCodes = fmap S.findMax
  . head
  . dropWhile (any ((> 1).S.size))
  . iterate removeSingles
  . M.fromListWith (\s1 s2-> S.intersection s2 s1)
  . map (\(i,o) -> (i,S.fromList o))

removeSingles :: Map Integer (S.Set (OpCode,OpType)) -> Map Integer (S.Set (OpCode,OpType))
removeSingles m = fmap (\s -> if S.size s > 1 then foldr S.delete s singles else s) m
  where
    singles = fmap S.findMin $ M.elems $ M.filter ((== 1).S.size) m

runProgram :: Map Integer (OpCode,OpType) -> S -> [ProgramLine] -> S
runProgram ops = foldl' go
      where
        go r (num,vs) = runCode r o t vs
          where (o,t) = ops M.! num

part1Sol :: [Input] -> [(Integer, [(OpCode, OpType)])]
part1Sol inputs = filter ((>= 3).length.snd) $ map possibleCodes inputs

part2Sol :: [Input] -> [ProgramLine] -> Integer
part2Sol inputs program = runProgram ops irs program M.! 0
    where
      irs = M.fromList $ zip [0,1,2,3] (repeat 0)
      ops = getCodes $ map possibleCodes inputs

part1 :: IO Int
part1= length . part1Sol <$> parseInput
part2 :: IO Integer
part2= part2Sol <$> parseInput <*> parseProg

test :: Text
test = "Before: [3, 2, 1, 1]\n9 2 1 2\nAfter:  [3, 2, 2, 1]"
