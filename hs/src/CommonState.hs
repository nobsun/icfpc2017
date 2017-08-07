{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CommonState
  ( MovePool
  , empty
  , applyMoves
  , riversOf
  , reachabilityOf
  , scoreOf
  , scores
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

data MovePool = MovePool { pool :: IM.IntMap Entry } deriving (Show, Generic)

type Entry = (Set NRiver, UF.Table)

emptyEntry :: Entry
emptyEntry = (Set.empty, UF.emptyTable)

empty :: MovePool
empty = MovePool { pool = IM.empty }

applyMoves :: [P.Move] -> MovePool -> MovePool
applyMoves moves (MovePool {pool = pl}) = MovePool $ foldl' upsert pl moves

upsert :: IM.IntMap (Set NRiver, UF.Table) -> P.Move -> IM.IntMap (Set NRiver, UF.Table)
upsert pl (P.MvClaim p s t) = IM.insert p (Set.insert (toNRiver' s t) rs, UF.unify e s t) pl
  where
    (rs, e) = IM.findWithDefault emptyEntry p pl
upsert pl (P.MvPass _) = pl
upsert pl (P.MvSplurge p ss) = IM.insert p (rs `Set.union` Set.fromList [toNRiver' s t | (s,t) <- rs2], UF.unifyN e rs2) pl
  where
    (rs, e) = IM.findWithDefault emptyEntry p pl
    rs2 = zip ss (tail ss)

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
