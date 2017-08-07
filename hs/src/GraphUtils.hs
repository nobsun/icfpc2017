{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GraphUtils
  ( initGraph
  , initSubGraph
  , updateGraph0
  , updateGraph1
  , insClaimedRiver0
  , insClaimedRiver1
  ) where

import Control.Arrow
-- import GHC.Generics
import Data.Aeson
-- import Data.Aeson.Types
import Data.Graph.Inductive.Basic as G
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree
import Protocol as P

{- |
Map から グラフ を構成する
ノードラベルは論理値でサイトがマインならTrueそれ以外ならFalse
エッジラベルはPunterId
>>> :set -XOverloadedStrings
>>> import Protocol as P
>>> let exSetupS = "{\"punter\":1,\"punters\":2,\"map\":{\"sites\":[{\"id\":4,\"x\":2.0,\"y\":-2.0},{\"id\":1,\"x\":1.0,\"y\":0.0},{\"id\":3,\"x\":2.0,\"y\":-1.0},{\"id\":6,\"x\":0.0,\"y\":-2.0},{\"id\":5,\"x\":1.0,\"y\":-2.0},{\"id\":0,\"x\":0.0,\"y\":0.0},{\"id\":7,\"x\":0.0,\"y\":-1.0},{\"id\":2,\"x\":2.0,\"y\":0.0}],\"rivers\":[{\"source\":3,\"target\":4},{\"source\":0,\"target\":1},{\"source\":2,\"target\":3},{\"source\":1,\"target\":3},{\"source\":5,\"target\":6},{\"source\":4,\"target\":5},{\"source\":3,\"target\":5},{\"source\":6,\"target\":7},{\"source\":5,\"target\":7},{\"source\":1,\"target\":7},{\"source\":0,\"target\":7},{\"source\":1,\"target\":2}],\"mines\":[1,5]},\"state\":{}}"
>>> let (Just exGraph) = initGraph <$> P.map <$> (decode exSetupS :: Maybe Setup)
>>> exGraph
mkGraph [(0,False),(1,True),(2,False),(3,False),(4,False),(5,True),(6,False),(7,False)] [(0,1,-1),(0,7,-1),(1,0,-1),(1,2,-1),(1,3,-1),(1,7,-1),(2,1,-1),(2,3,-1),(3,1,-1),(3,2,-1),(3,4,-1),(3,5,-1),(4,3,-1),(4,5,-1),(5,3,-1),(5,4,-1),(5,6,-1),(5,7,-1),(6,5,-1),(6,7,-1),(7,0,-1),(7,1,-1),(7,5,-1),(7,6,-1)]
-}

initGraph :: Map -> Gr Bool PunterId
initGraph (Map ss rs ms) = G.gmap ini $ G.undir $ mkGraph (Prelude.map (lnode . deSite) ss) (Prelude.map (ledge  . deRiver) rs)
  where
    ini (ins, n, _, outs) = ( Prelude.map (first (const nopunter)) ins
                            , n, n `elem` ms
                            , Prelude.map (first (const nopunter)) outs)
    nopunter = -1

deSite :: Site -> SiteId
deSite (Site i) = i

deRiver :: River -> (SiteId, SiteId)
deRiver (River s t) = (s, t)

lnode :: SiteId -> LNode ()
lnode i = (i,())

ledge :: (SiteId, SiteId) -> LEdge ()
ledge (s,t) = (s,t,())

instance (ToJSON a, ToJSON b) => ToJSON (Gr a b) where
  toJSON g = object ["nodes" .= nodes g, "edges" .= edges g]
instance (FromJSON a, FromJSON b) => FromJSON (Gr a b) where
  parseJSON (Object v) = mkGraph <$> v .: "nodes" <*> v .: "edges"
  parseJSON _          = return G.empty

{- |
Map から頂点のみからなる部分グラフを構成する
-}

initSubGraph :: Map -> Gr Bool PunterId
initSubGraph (Map ss _ ms) = G.gmap ini $ G.undir $ mkGraph (Prelude.map (lnode . deSite) ss) []
  where
    ini (ins, n, _, outs) = ( ins
                            , n, n `elem` ms
                            , outs)

{- |
直前ターンの参加者の手のリスト PrevMoves で Graph を更新
取られたRiverを消す
-}

updateGraph0 :: PrevMoves a -> Gr Bool PunterId -> Gr Bool PunterId
updateGraph0 (PrevMoves (Moves ms) _) gr
  = foldr update gr ms
    where
      update (MvClaim _ s t) g = delEdges [(s,t),(t,s)] g
      update _ g = g

{- |
直前ターンの参加者の手のリスト PrevMoves で Graph を更新
取られたRiverにPunterIdを設定
-}

updateGraph1 :: PrevMoves a -> Gr Bool PunterId -> Gr Bool PunterId
updateGraph1 pms@(PrevMoves (Moves ms) _) gr
  = foldr update gr ms
    where
      update (MvClaim pid s t) g = insEdges [(s,t,pid),(t,s,pid)] $ updateGraph0 pms g
      update _ g = g

{- |
獲得したエッジを部分グラフに追加
もともと当該エッジがないことが前提
-}

insClaimedRiver0 :: Move -> Gr Bool PunterId -> Gr Bool PunterId
insClaimedRiver0 (MvClaim pid s t)  = insEdges [(s,t,pid),(t,s,pid)]
insClaimedRiver0 _                  = Prelude.id

{- |
獲得したエッジを部分グラフに追加
もともと当該エッジが存在しそのエッジにPunterIdを設定する
-}

insClaimedRiver1 :: Move -> Gr Bool PunterId -> Gr Bool PunterId
insClaimedRiver1 (MvClaim pid s t)  = insEdges [(s,t,pid),(t,s,pid)] . delEdges [(s,t),(t,s)]
insClaimedRiver1 _                  = Prelude.id


