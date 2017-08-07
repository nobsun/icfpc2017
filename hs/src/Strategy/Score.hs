module Strategy.Score
  (
    -- 候補手グループを取得しすぎないように注意
    greedyDiffs,
    greedyScores,
  ) where

-- score を最大化する候補手

import Control.Arrow (second)
import Data.Ord (Down (..))
import Data.List (unfoldr, group)
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

greedyDiffs :: ScoreTable            -- ^ mine 毎の、site 毎の score のマップ
            -> UF.Table              -- ^ 到達可能 site 集合用 query table
            -> Set NRiver            -- ^ 絞り込み対象候補手の集合
            -> [([NRiver], Integer)] -- ^ スコア上昇値順 候補手グループ
greedyDiffs scoreTbl classes ars =
    map (second (subtract curScore)) $ greedyScores scoreTbl classes ars
  where
    curScore = ScoreTable.computeScore scoreTbl classes

greedyScores :: ScoreTable          -- ^ mine 毎の、site 毎の score のマップ
             -> UF.Table            -- ^ 到達可能 site 集合用 query table
             -> Set NRiver          -- ^ 絞り込み対象候補手の集合
             -> [([NRiver], Score)] -- ^ スコア上位順 候補手グループ
greedyScores scoreTbl classes ars =
    foldr aggregate [] .     -- [[(NRiver, Score)]] -> [([NRiver], Score)]
    map (map unScoreOrd) .
    group .
    map unDown .
    unfoldr Heap.viewMin $   -- Heap (Down ScoreOrd) -> [Down ScoreOrd]
    Heap.fromList
    [ Down $ ScoreOrd (r, ScoreTable.computeScore scoreTbl $ UF.unify classes s t)
    | r <- Set.toList ars, let (s,t) = deNRiver r]
  where
    unDown (Down x) = x
    aggregate (xs@((_rv, sc):_)) ys = (map fst xs, sc) : ys
    aggregate  []                ys =  ys
