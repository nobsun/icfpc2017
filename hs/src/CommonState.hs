{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CommonState
  ( MovePool
  , empty
  , applyMove
  , applyMoves
  , riversOf
  , reachabilityOf
  , scoreOf
  , scores
  , unclaimedRivers
  , optionableRivers -- オプション機能が有効になっているかチェックしていないので注意
  ) where

import Data.List (foldl')
import qualified Data.Aeson as J
import GHC.Generics
import qualified Data.IntMap.Lazy as IM
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Protocol as P
import qualified UnionFind as UF
import qualified ScoreTable as ScoreTable
import NormTypes


data MovePool
  = MovePool
  { unclaimedRivers :: Set NRiver
  , optionableRivers :: Set NRiver
  , pool :: IM.IntMap Entry
  } deriving (Show, Generic)

type Entry = (Set NRiver, UF.Table)

emptyEntry :: Entry
emptyEntry = (Set.empty, UF.emptyTable)

empty :: P.Map -> MovePool
empty m =
  MovePool
  { unclaimedRivers = Set.fromList $ map toNRiver $ P.rivers m
  , optionableRivers = Set.empty
  , pool = IM.empty
  }

applyMoves :: [P.Move] -> MovePool -> MovePool
applyMoves moves pl = foldl' (flip applyMove) pl moves

applyMove :: P.Move -> MovePool -> MovePool
applyMove (P.MvPass _) pl = pl
applyMove (P.MvClaim p s t) MovePool{ unclaimedRivers = urs, optionableRivers = ors, pool = pl } =
  MovePool
  { unclaimedRivers = Set.delete r urs
  , optionableRivers = Set.insert r ors
  , pool = IM.insert p (Set.insert r rs, UF.unify e s t) pl
  }
  where
    (rs, e) = IM.findWithDefault emptyEntry p pl
    r = toNRiver' s t
applyMove (P.MvOption p s t) orig@MovePool{ optionableRivers = ors, pool = pl } =
  orig
  { optionableRivers = Set.delete r ors
  , pool = IM.insert p (Set.insert r rs, UF.unify e s t) pl
  }
  where
    (rs, e) = IM.findWithDefault emptyEntry p pl
    r = toNRiver' s t
applyMove (P.MvSplurge p ss) MovePool{ unclaimedRivers = urs, optionableRivers = ors, pool = pl } =
  MovePool
  { unclaimedRivers = urs Set.\\ rs2'
  , optionableRivers = ors Set.\\ (rs2' Set.\\ urs)
  , pool = IM.insert p (rs `Set.union` rs2', UF.unifyN e rs2) pl
  }
  where
    (rs, e) = IM.findWithDefault emptyEntry p pl
    rs2 = zip ss (tail ss)
    rs2' = Set.fromList $ map (uncurry toNRiver') rs2

instance J.ToJSON MovePool
instance J.FromJSON MovePool

riversOf :: MovePool -> P.PunterId -> Set NRiver
riversOf MovePool{ pool = pl } pid = fst $ IM.findWithDefault emptyEntry pid pl

reachabilityOf :: MovePool -> P.PunterId -> UF.Table
reachabilityOf MovePool{ pool = pl } pid = snd $ IM.findWithDefault emptyEntry pid pl

scoreOf :: MovePool -> ScoreTable.ScoreTable -> P.PunterId -> ScoreTable.Score
scoreOf MovePool{ pool = pl } tbl pid = ScoreTable.computeScore tbl (snd (IM.findWithDefault emptyEntry pid pl))

scores :: MovePool -> ScoreTable.ScoreTable -> IM.IntMap ScoreTable.Score
scores MovePool{ pool = pl } tbl = fmap (ScoreTable.computeScore tbl . snd) pl
