{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- 取得済み(距離0扱い)or取得可能(距離1扱い)なエッジだけからなるグラフ上で、
-- 各マインからのスパニングツリーを作って、
-- それらのスパニングツリーに含まれる回数の多いものを選択するという戦略。
module Punter.STF where

import Control.Monad
import qualified Data.Aeson as J
import Data.List (maximumBy)
import Data.Ord
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Generics

import qualified CommonState as CS
import qualified Protocol as P
import Punter
import NormTypes
import Dijkstra

data Punter
  = Punter
  { setupInfo :: P.Setup
  , movePool :: CS.MovePool
  }
  deriving (Generic)

instance J.ToJSON Punter
instance J.FromJSON Punter

instance Punter.IsPunter Punter where
  setup s =
    P.ReadyOn
    { P.ready   = P.setupPunter s
    , P.state   = Just $
        Punter
        { setupInfo = s
        , movePool = CS.empty m
        }
    , P.futures = Nothing
    }
    where
      m = P.map s

  applyMoves (P.Moves moves) p1@Punter{ movePool = movePool1 } =
    p1
    { movePool = CS.applyMoves moves movePool1
    }

  chooseMoveSimple Punter{ setupInfo = si, movePool = pool }
    | Set.null ars = P.MvPass punterId
    | Map.null tbl = P.MvPass punterId
    | otherwise =
        let (s,t) = deNRiver $ fst $ maximumBy (comparing snd) (Map.toList tbl)
        in P.MvClaim punterId s t
    where
      punterId = P.setupPunter si
      ars = CS.unclaimedRivers pool
      mrs = CS.riversOf pool punterId

      tbl :: Map NRiver Int
      tbl = Map.fromListWith (+) $ do
        mine <- P.mines (P.map si)
        (_, Just (_, r)) <- HashMap.elems (dijkstra g [mine])
        guard $ r `Set.member` ars -- 既に所有しているものは候補に入れない
        return (r, 1)

      g :: HashMap P.SiteId [(P.SiteId, Integer, NRiver)]
      g = HashMap.fromListWith (++) $
            [e | r <- Set.toList ars, let (s,t) = deNRiver r, e <- [(s, [(t,1,r)]), (t, [(s,1,r)])]] ++
            [e | r <- Set.toList mrs, let (s,t) = deNRiver r, e <- [(s, [(t,0,r)]), (t, [(s,0,r)])]]
