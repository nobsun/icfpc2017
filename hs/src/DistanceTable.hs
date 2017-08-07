module DistanceTable
  ( DistanceTable
  , Distance
  , mkDistanceTable
  , mineDistances
  ) where

import Data.Ord (comparing)
import Data.List (minimumBy)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Dijkstra
import qualified Protocol as P
import qualified UnionFind as UF

type Distance = Int

type DistanceTable = IntMap (IntMap Distance)

mkDistanceTable :: P.Map -> DistanceTable
mkDistanceTable m = IntMap.fromList
  [ (mine, IntMap.fromList [(site, d) | (site, (d, _)) <- HashMap.toList (dijkstra g [mine])])
  | mine <- P.mines m
  ]
  where
    g :: HashMap P.SiteId [(P.SiteId, Int, ())]
    g = HashMap.fromListWith (++) [e | P.River src tgt <- P.rivers m, e <- [(src, [(tgt, 1, ())]), (tgt, [(src, 1, ())])]]

mineDistances :: DistanceTable
              -> UF.Table
              -> [P.SiteId]  -- ^ 検索起点 site 集合
              -> [(Distance, P.SiteId)]
mineDistances table uf sss = do
  (mine, tbl) <- IntMap.toList table
  ss <- sss
  return $ minimumBy (comparing fst) [(IntMap.findWithDefault maxBound site tbl, mine) | site <- UF.classToList (UF.getClass uf ss)]

_testMap :: P.Map
_testMap =
  P.Map
  { P.sites = map P.Site [ 1, 2, 3, 4 ]
  , P.rivers = [ P.River 1 2, P.River 1 3, P.River 2 3, P.River 2 4 ]
  , P.mines = [1]
  }

_testMkDistance :: DistanceTable
_testMkDistance = mkDistanceTable _testMap

_testMineDistances :: [Bool]
_testMineDistances =
  [ mineDistances _testMkDistance (UF.unify UF.emptyTable 2 4) [4] == [(1, 1)]
  , mineDistances _testMkDistance UF.emptyTable [4] == [(2, 1)]
  ]
