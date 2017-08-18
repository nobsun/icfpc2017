module DistanceTable
  ( DistanceTable
  , Distance
  , mkDistanceTable
  , mineDistances

  , Score
  , Futures
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

type Futures = IntMap P.SiteId

computeScore :: DistanceTable -> Futures -> UF.Table -> Score
computeScore table futures uf = sum $ do
  (mine, tbl) <- IntMap.toList table
  let score =
        sum
        [ d ^ (2::Int)
        | site <- UF.classToList (UF.getClass uf mine)
        , let d = fromIntegral (IntMap.findWithDefault 0 site tbl)
        ]
      fscore =
        case IntMap.lookup mine futures of
          Nothing -> 0
          Just site ->
            (if UF.areSameClass uf mine site then id else negate)
            (fromIntegral (tbl IntMap.! site) ^ (3::Int))
  return $ score + fscore

-- NRiver を追加することによるスコアの増分
reward :: DistanceTable -> Futures -> UF.Table -> NRiver -> Score
reward table futures equiv r
  | s' == t'  = 0
  | otherwise =
      sum
        [ d ^ (2::Int) + (if IntMap.lookup s'' futures == Just t'' then 2 * d ^ (3::Int) else 0)
        | s'' <- UF.classToList (UF.getClass equiv s')
        , tbl2 <- maybeToList $ IntMap.lookup s'' table
        , t'' <- UF.classToList (UF.getClass equiv t')
        , let d = fromIntegral (tbl2 IntMap.! t'')
        ]
      +
      sum
        [ d ^ (2::Int) + (if IntMap.lookup s'' futures == Just t'' then 2 * d ^ (3::Int) else 0)
        | t'' <- UF.classToList (UF.getClass equiv t')
        , tbl2 <- maybeToList $ IntMap.lookup t'' table
        , s'' <- UF.classToList (UF.getClass equiv s')
        , let d = fromIntegral (tbl2 IntMap.! s'')
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
