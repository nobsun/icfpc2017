{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CommonState where

import qualified Data.Aeson as J
import GHC.Generics
import Data.IntMap.Lazy as IM
import qualified UnionFind as UF

data MovePool = MovePool { pool :: IntMap UF.Table } deriving (Show, Generic)

empty :: MovePool
empty = MovePool { pool = IM.empty }

instance J.ToJSON MovePool
instance J.FromJSON MovePool
