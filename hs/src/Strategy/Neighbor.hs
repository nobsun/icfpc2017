module Strategy.Neighbor
  (
    -- 候補手グループを取得しすぎないように注意
    neighborRivers,
  ) where

-- 到達可能 site からの (到達していない) mine への距離 を最小化する候補手

import Control.Arrow
import Data.Ord (comparing, Down (..))
import Data.List (unfoldr, group, sortBy)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Heap as Heap

import qualified Protocol as P
import NormTypes (NRiver, deNRiver, toNRiver')
import qualified UnionFind as UF
import DistanceTable (DistanceTable, Distance)
import qualified DistanceTable as DistanceTable


newtype DistanceOrd = DistanceOrd { unDistanceOrd :: (NRiver, Int) }

instance Eq DistanceOrd where
  DistanceOrd x == DistanceOrd y  =  snd x == snd y

instance Ord DistanceOrd where
  DistanceOrd x `compare` DistanceOrd y  =  snd x `compare` snd y

neighborRivers :: DistanceTable                 -- ^ mine 毎の、site 毎の score のマップ
               -> UF.Table                      -- ^ 到達可能 site 集合用 query table
               -> Set NRiver                    -- ^ 絞り込み対象候補手の集合
               -> [([(NRiver, Int)], Distance)] {- ^ mine に近い順 候補手と近接mine数のグループ
                                                     グループ内は近接 mine 数の多い順
                                                     mine を端点とする川が最初に出るので注意 -}
neighborRivers distTbl classes ars =
    map (first (sortBy (comparing $ Down . snd) . foldr aggsize [] . group)) .
    foldr aggregate [] .
    map (map unDistanceOrd) .
    group .
    unfoldr Heap.viewMin $   -- Heap (Down ScoreOrd) -> [Down ScoreOrd]
    Heap.fromList
    [ DistanceOrd (r, d)
    | r <- Set.toList ars
    , let (s,t) = deNRiver r
    , (d, _) <- DistanceTable.mineDistances distTbl (UF.unify classes s t) [s, t]
    ]
  where
    aggregate (xs@((_rv, sc):_)) ys = (map fst xs, sc) : ys
    aggregate  []                ys =  ys
    aggsize (xs@(x:_)) ys = (x, length xs) : ys
    aggsize  []        ys =  ys


_testAvailRivers' :: [(P.SiteId, P.SiteId)]
_testAvailRivers' = [ (1, 2), (1, 3), (2, 3), (2, 4) ]

_testMap :: P.Map
_testMap =
  P.Map
  { P.sites = map P.Site [ 1, 2, 3, 4 ]
  , P.rivers = map (uncurry P.River) _testAvailRivers'
  , P.mines = [1, 4]
  }

_testAvailRivers :: Set NRiver
_testAvailRivers = Set.fromList $ map (uncurry toNRiver') _testAvailRivers'

_testMkDistance :: DistanceTable
_testMkDistance = DistanceTable.mkDistanceTable _testMap

_testNeighborRivers :: [([(NRiver, Int)], Distance)]
_testNeighborRivers = neighborRivers _testMkDistance UF.emptyTable _testAvailRivers
