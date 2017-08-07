module Strategy.Degree
  (
    -- 候補手グループを取得しすぎないように注意
    higherDegrees,
    computeDegree,
  ) where

-- 到達可能 site を増やす候補手の数 Degree を最大化する候補手

import Control.Monad
import Data.Ord (Down (..))
import Data.List (unfoldr, group)
import qualified Data.IntSet as IntSet
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Heap as Heap

import Protocol (SiteId)
import NormTypes (NRiver, deNRiver)
import qualified UnionFind as UF


newtype DegreeOrd = DegreeOrd { unDegreeOrd :: (NRiver, Int) }

instance Eq DegreeOrd where
  DegreeOrd x == DegreeOrd y  =  snd x == snd y

instance Ord DegreeOrd where
  DegreeOrd x `compare` DegreeOrd y  =  snd x `compare` snd y

higherDegrees :: [SiteId]          -- ^ λ鉱山のリスト
              -> Set NRiver        -- ^ 取得可能 River 集合全体
              -> UF.Table          -- ^ 到達可能 site 集合
              -> Set NRiver        -- ^ 絞り込み対象候補手の集合
              -> [([NRiver], Int)] -- ^ Degree 上位順 候補手グループ
higherDegrees mines ars siteClasses crs =
    foldr aggregate [] .     -- [[(NRiver, Int)]] -> [([NRiver], Int)]
    map (map unDegreeOrd) .
    group .
    map unDown .
    unfoldr Heap.viewMin $   -- Heap (Down DegreeOrd) -> [Down DegreeOrd]
    Heap.fromList
    [ Down $ DegreeOrd (r, computeDegree mines (Set.delete r ars) (UF.unify siteClasses s t))
    | r <- Set.toList crs
    , let (s,t) = deNRiver r
    ]
  where
    unDown (Down x) = x
    aggregate (xs@((_rv, sc):_)) ys = (map fst xs, sc) : ys
    aggregate  []                ys =  ys

computeDegree :: [SiteId]   -- ^ λ鉱山のリスト
              -> Set NRiver -- ^ 取得可能 River 集合全体
              -> UF.Table   -- ^ 到達可能 site 集合用 query table
              -> Int        -- ^ 結果候補手数
computeDegree mines ars siteClasses = length $ do
  mine <- mines
  let sites = IntSet.fromList $ UF.classToList $ UF.getClass siteClasses mine
  nr  <- Set.toList ars
  let (s,t) = deNRiver nr
  -- エッジの一方の頂点だけがsitesに含まれる
  guard $ (s `IntSet.member`) sites /= (t `IntSet.member`) sites
  return ()
