module Strategy.Score
  ( greedyScores
  , greedyScores'
  ) where

import Data.Ord (Down (..))
import Data.List (unfoldr)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Heap as Heap

import NormTypes (NRiver, deNRiver)
import qualified UnionFind as UF
import ScoreTable (ScoreTable, Score)
import qualified ScoreTable as ScoreTable


newtype ScoreOrd = ScoreOrd { unScoreOrd :: (NRiver, Score) }

instance Eq ScoreOrd where
  ScoreOrd x == ScoreOrd y  =  snd x == snd y

instance Ord ScoreOrd where
  ScoreOrd x `compare` ScoreOrd y  =  snd x `compare` snd y

greedyScores :: Int                -- スコア上位 n個
             -> ScoreTable         -- mine 毎の、site 毎の score のマップ
             -> UF.Table           -- mine 毎の到達可能 site 集合
             -> Set NRiver         -- 取得可能の River の集合
             -> [(NRiver, Score)]  -- スコア上位 n個の取得候補
greedyScores n scoreTbl classes ars =
  take n $ greedyScores' scoreTbl classes ars

greedyScores' :: ScoreTable
              -> UF.Table
              -> Set NRiver
              -> [(NRiver, Score)]
greedyScores' scoreTbl classes ars =
    map (unScoreOrd . unDown) .
    unfoldr Heap.viewMin $
    Heap.fromList
    [ Down $ ScoreOrd (r, ScoreTable.computeScore scoreTbl $ UF.unify classes s t)
    | r <- Set.toList ars, let (s,t) = deNRiver r]
  where
    unDown (Down x) = x
