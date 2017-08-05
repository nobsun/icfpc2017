{-# OPTIONS_GHC -Wall #-}
module ScoreTable
  ( ScoreTable
  , mkScoreTable
  , computeScore
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Dijkstra
import qualified Protocol as P
import qualified UnionFind as UF

type Score = Integer

type ScoreTable = IntMap (IntMap Score)

mkScoreTable :: P.Map -> ScoreTable
mkScoreTable m = IntMap.fromList
  [ (mine, IntMap.fromList [(site,d*d) | (site, (d, _)) <- HashMap.toList (dijkstra g [mine])])
  | mine <- P.mines m
  ]
  where
    g :: HashMap P.SiteId [(P.SiteId, Integer, ())]
    g = HashMap.fromListWith (++) [e | P.River src tgt <- P.rivers m, e <- [(src, [(tgt, 1, ())]), (tgt, [(src, 1, ())])]]

computeScore :: ScoreTable -> UF.Table -> Score
computeScore table uf = sum $ do
  (mine, tbl) <- IntMap.toList table
  return $ sum [IntMap.findWithDefault 0 site tbl | site <- UF.classToList (UF.getClass uf mine)]
