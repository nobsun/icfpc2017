{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CommonState where

import Data.List (foldl')
import qualified Data.Aeson as J
import GHC.Generics
import qualified Data.IntMap.Lazy as IM
import qualified Protocol as P
import qualified UnionFind as UF

data MovePool = MovePool { pool :: IM.IntMap UF.Table } deriving (Show, Generic)

empty :: MovePool
empty = MovePool { pool = IM.empty }

applyMoves :: [P.Move] -> MovePool -> MovePool
applyMoves moves (MovePool {pool = pl}) = MovePool $ foldl' upsert pl moves

upsert :: IM.IntMap UF.Table -> P.Move -> IM.IntMap UF.Table
upsert pl (P.MvClaim p s t) = IM.insert p (UF.unify (IM.findWithDefault UF.emptyTable p pl) s t) pl
upsert pl (P.MvPass _) = pl

instance J.ToJSON MovePool
instance J.FromJSON MovePool
