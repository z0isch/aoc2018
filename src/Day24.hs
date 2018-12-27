{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Day24 where

import Debug.Trace
import Data.Monoid
import Data.Void
import Control.Monad
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO
import Data.Maybe
import Control.Lens
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Data.Function
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Bifoldable
import Data.Either
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Foldable

type P = Parsec Void Text

data AttackType = Fire | Cold | Slashing | Radiation | Bludgeoning
    deriving (Eq,Show,Ord)
makePrisms ''AttackType

data Unit = Unit
    { _uHp :: !Integer
    , _uAd :: !Integer
    , _uAt :: !AttackType
    , _uInit :: !Integer
    , _uW :: !(Set AttackType)
    , _uImm :: !(Set AttackType)
    }
    deriving (Eq, Show, Ord)
makeLenses ''Unit

data Group = Group 
    { _gNum :: !Integer
    , _gUnit :: !Unit
    , _gId :: !Integer
    } 
    deriving (Eq, Show, Ord)
makeLenses ''Group

type S = Map Integer (Either Group Group)

parseInput :: IO S
parseInput = parse totP "" <$> TIO.readFile "./input/day24.txt" >>= \case
    Left err -> error (parseErrorPretty err)
    Right x -> pure x

totP :: P S
totP = (\imm inf -> M.union (M.fromList $ zipWith (\i (n, u) -> (i,Left $ Group n u i)) [0..] imm) (M.fromList $ zipWith (\i (n, u) -> (i,Right $ Group n u i)) [(genericLength imm)..] inf))
    <$ "Immune System:\n" <*> someTill (gP <* "\n") "\n"
    <* "Infection:\n" <*> (gP `sepBy` "\n")
    <* eof
gP = (,) <$> L.decimal <* " units each with " <*> uP 
uP = (\hp ms ad at i -> Unit hp ad at i (maybe mempty (maybe mempty S.fromList . fst) ms) (maybe mempty (maybe mempty S.fromList . snd) ms))
    <$> L.decimal <* " hit points " 
    <*> (optional (between "(" ") " (
            (,) <$> optional wP <* optional "; " <*> optional iP
        )))
    <* "with an attack that does " <*> L.decimal <* " " <*> atP 
    <* " damage at initiative " <*> L.decimal
iP = "immune to " *> (atP `sepBy` ", ")
wP = "weak to " *> (atP `sepBy` ", ")
atP = Fire <$ "fire" <|> Cold <$ "cold" <|> Slashing <$ "slashing" <|> Radiation <$ "radiation" <|> Bludgeoning <$ "bludgeoning"

targetSelect :: Group -> Map Integer Group -> Maybe Group
targetSelect g gs
    | M.null gs = Nothing
    | otherwise = if dmg g t == 0 then Nothing else Just t
    where
        t = maximumBy (compare `on` orderG) gs
        orderG g' = (dmg g g', effectivePower g', g'^.gUnit.uInit)

attack :: Group -> Group -> Group
attack g1 g2 = g2 & gNum %~ \u -> u - unitsLost
    where unitsLost = (dmg g1 g2) `div` (g2^.gUnit.uHp)

effectivePower :: Group -> Integer
effectivePower g = (g^.gNum) * (g^.gUnit.uAd)

dmg :: Group -> Group -> Integer
dmg g1 g2
    | immune = 0
    | weak = dmg * 2
    | otherwise = dmg
    where 
        dmg = effectivePower g1
        immune = (g1^.gUnit.uAt) `S.member` (g2^.gUnit.uImm)
        weak = (g1^.gUnit.uAt) `S.member` (g2^.gUnit.uW)

doTurn :: S -> S
doTurn s = foldl' update s $ Seq.reverse $ Seq.sortBy (compare `on` initOrder) $ foo s
    where
        initOrder (g,_) = maybe (-100) (view (gUnit.uInit) . either id id) $ M.lookup g s
        update s' (gid1,gid2) = case (,) <$> M.lookup gid1 s' <*> M.lookup gid2 s' of
            Nothing -> s'
            Just (g1,g2) -> let g2' = attack (either id id g1) (either id id g2) in 
                if g2'^.gNum <= 0 
                then M.delete (g2'^.gId) s'
                else M.update (\case Left _-> Just (Left g2'); Right _-> Just (Right g2')) (g2'^.gId) s'

foo :: S -> Seq (Integer,Integer)
foo s = targetOrder (mempty, s, s)
    where
        targetOrder (paired,attackers,defenders)
            | M.null attackers = paired
            | otherwise = targetOrder (paired', attackers', defenders')
            where
                attacker = maximumBy (compare `on` orderEG) attackers
                a = either id id attacker
                attackers' = M.delete (a^.gId) attackers
                differentFromAttacker = case attacker of
                    Left _ -> isRight
                    _ -> isLeft
                defender = targetSelect a (either id id <$> M.filter differentFromAttacker defenders)
                paired' = case defender of
                    Nothing -> paired
                    Just d -> paired Seq.|> (a^.gId,d^.gId)
                defenders' =  case defender of 
                    Nothing -> defenders
                    Just d -> M.delete (d^.gId) defenders
        orderG g = (effectivePower g, g^.gUnit.uInit)
        orderEG = \case 
            Left g -> orderG g
            Right g -> orderG g

part1Sol = 
    foldMap (bifoldMap (Sum . view gNum) (Sum . view gNum)) .
    head . 
    dropWhile (\s -> let (x,y) = M.partition isLeft s in not (M.null x || M.null y))
    . iterate doTurn

part1 = part1Sol <$> parseInput

part2Sol = 
    foldMap (bifoldMap (Sum . view gNum) (Sum . (* (-1)). view gNum)) .
    head . 
    dropWhile (\s -> let (x,y) = M.partition isLeft s in not (M.null x || M.null y))
    . iterate doTurn
part2 = part2Sol . boost 29 <$> parseInput

boost :: Integer -> S -> S
boost b s = M.foldrWithKey (\k g s' -> case g of Left i-> M.update (const $ Just $ Left $ i & gUnit.uAd %~ (+ b)) k s'; Right i -> s';) s s

test :: S
test = fromJust $ parseMaybe totP "Immune System:\n17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n989 units each with 1274 hit points (weak to bludgeoning, slashing; immune to fire) with an attack that does 25 slashing damage at initiative 3\n\nInfection:\n801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n4485 units each with 2961 hit points (weak to fire, cold; immune to radiation) with an attack that does 12 slashing damage at initiative 4"