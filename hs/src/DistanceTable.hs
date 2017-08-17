module DistanceTable
  ( DistanceTable
  , Distance
  , mkDistanceTable
  , mineDistances

  , Score
  , computeScore
  , reward
  ) where

import Data.Ord (comparing)
import Data.List (minimumBy)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe

import Dijkstra
import qualified Protocol as P
import qualified UnionFind as UF
import NormTypes

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
  return . minimumBy (comparing fst) $ do
    ss <- sss
    site <- UF.classToList (UF.getClass uf ss)
    return (IntMap.findWithDefault maxBound site tbl, mine)

type Score = Integer

computeScore :: DistanceTable -> UF.Table -> Score
computeScore table uf = sum $ do
  (mine, tbl) <- IntMap.toList table
  return $ sum [fromIntegral (IntMap.findWithDefault 0 site tbl) ^ (2::Int) | site <- UF.classToList (UF.getClass uf mine)]

-- NRiver を追加することによるスコアの増分
reward :: DistanceTable -> UF.Table -> NRiver -> Score
reward table equiv r
  | s' == t'  = 0
  | otherwise = 
      sum
        [ fromIntegral (tbl2 IntMap.! t'') ^ (2::Int)
        | s'' <- UF.classToList (UF.getClass equiv s')
        , tbl2 <- maybeToList $ IntMap.lookup s'' table
        , t'' <- UF.classToList (UF.getClass equiv t')
        ]
      +
      sum
        [ fromIntegral (tbl2 IntMap.! s'') ^ (2::Int)
        | t'' <- UF.classToList (UF.getClass equiv t')
        , tbl2 <- maybeToList $ IntMap.lookup t'' table
        , s'' <- UF.classToList (UF.getClass equiv s')
        ]
  where
    (s,t) = deNRiver r
    s' = UF.getRepr equiv s
    t' = UF.getRepr equiv t

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
