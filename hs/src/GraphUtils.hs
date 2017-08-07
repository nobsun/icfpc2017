{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GraphUtils
  ( initCity
  , updateCity
  , updateCityByMove
  , toAvailableRivers
  , toLocalMap
  , toLocalMapTable
  , initAvailableRivers
  , initLocalMapTable
  , updateAvailableRivers
  , updateLocalMapTable
  , updateLocalMap
  ) where

import Data.Bool
import GHC.Generics
import Data.Aeson
import Data.Graph.Inductive.Basic as G
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.Query.BFS as G
import Data.Graph.Inductive.PatriciaTree
import Protocol as P
import Data.List as L
import Data.IntMap.Lazy (IntMap)
import qualified Data.IntMap.Lazy as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

instance (ToJSON a, ToJSON b) => ToJSON (Gr a b) where
  toJSON g = object ["nodes" .= nodes g, "edges" .= edges g]
instance (FromJSON a, FromJSON b) => FromJSON (Gr a b) where
  parseJSON (Object v) = mkGraph <$> v .: "nodes" <*> v .: "edges"
  parseJSON _          = return G.empty

{- |
Mapから取得可能なRiverを含むGraphを構成
ノードラベルは論理値でサイトがマインならTrueそれ以外ならFalse
エッジラベルは距離で常に1
>>> :set -XOverloadedStrings
>>> import Protocol as P
>>> let exSetupS = "{\"punter\":1,\"punters\":2,\"map\":{\"sites\":[{\"id\":4,\"x\":2.0,\"y\":-2.0},{\"id\":1,\"x\":1.0,\"y\":0.0},{\"id\":3,\"x\":2.0,\"y\":-1.0},{\"id\":6,\"x\":0.0,\"y\":-2.0},{\"id\":5,\"x\":1.0,\"y\":-2.0},{\"id\":0,\"x\":0.0,\"y\":0.0},{\"id\":7,\"x\":0.0,\"y\":-1.0},{\"id\":2,\"x\":2.0,\"y\":0.0}],\"rivers\":[{\"source\":3,\"target\":4},{\"source\":0,\"target\":1},{\"source\":2,\"target\":3},{\"source\":1,\"target\":3},{\"source\":5,\"target\":6},{\"source\":4,\"target\":5},{\"source\":3,\"target\":5},{\"source\":6,\"target\":7},{\"source\":5,\"target\":7},{\"source\":1,\"target\":7},{\"source\":0,\"target\":7},{\"source\":1,\"target\":2}],\"mines\":[1,5]},\"state\":{}}"
>>> let (Just exGraph) = initAvailablerivers <$> P.map <$> (decode exSetupS :: Maybe Setup)
>>> exGraph
mkGraph [(0,False),(1,True),(2,False),(3,False),(4,False),(5,True),(6,False),(7,False)] [(0,1,-1),(0,7,-1),(1,0,-1),(1,2,-1),(1,3,-1),(1,7,-1),(2,1,-1),(2,3,-1),(3,1,-1),(3,2,-1),(3,4,-1),(3,5,-1),(4,3,-1),(4,5,-1),(5,3,-1),(5,4,-1),(5,6,-1),(5,7,-1),(6,5,-1),(6,7,-1),(7,0,-1),(7,1,-1),(7,5,-1),(7,6,-1)]
-}

type City                = Gr Bool P.PunterId
type AvailableRiversGr a = Gr Bool a
type LocalMap a          = Gr SiteProp a
type LocalMapTable a     = IntMap (LocalMap a)

initCity :: P.Map -> City
initCity = G.emap (const nopunter) . initAvailableRivers

nopunter :: P.PunterId
nopunter = -1

updateCity :: P.PrevMoves a -> City -> City
updateCity (P.PrevMoves (P.Moves ms) _) city = foldr updateCityByMove city ms

updateCityByMove :: P.Move -> City -> City
updateCityByMove (MvClaim pid s t) city = G.insEdges [(s,t,pid),(t,s,pid)] $ G.delEdges [(s,t),(t,s)] city
updateCityByMove _                 city = city

toAvailableRivers :: City -> AvailableRiversGr ()
toAvailableRivers = G.emap (const ()) . G.elfilter (nopunter ==)

toLocalMap :: P.PunterId -> City -> LocalMap ()
toLocalMap pid city = L.foldr updateLocalMap city' mvs
  where
    city' = G.gmap conv $ G.emap (const ()) $ G.elfilter (pid ==) city
    conv (ins,v,l,outs) = (ins,v,bool Normal (Mine (IntSet.singleton v)) l,outs)
    mvs = zipWith mkMove (repeat pid) (G.edges city')
    mkMove p (s,t) = P.MvClaim p s t

toLocalMapTable :: Int -> City -> LocalMapTable ()
toLocalMapTable n city
  = IntMap.fromList $ L.map (conv city) [0 .. n-1]
    where
      conv cty pid = (pid, toLocalMap pid cty)

initAvailableRivers :: P.Map -> AvailableRiversGr ()
initAvailableRivers (P.Map ss rs ms) = G.gmap ini $ G.undir $ G.mkUGraph (L.map deSite ss) (L.map deRiver rs)
  where
    ini (ins, n, _, outs) = ( ins
                            , n, n `elem` ms
                            , outs)

data SiteProp = Mine IntSet | Pseudo IntSet | Normal deriving (Generic, Show)
instance ToJSON SiteProp
instance FromJSON SiteProp

deSite :: P.Site -> SiteId
deSite (P.Site i) = i

deRiver :: P.River -> (SiteId, SiteId)
deRiver (P.River s t) = (s, t)

{- |
Map から頂点のみからなる部分グラフを構成する
-}

initLocalMapTable :: Int -> P.Map -> LocalMapTable ()
initLocalMapTable np = IntMap.fromList . zip [0..np-1] . repeat . initLocalMap

initLocalMap :: P.Map -> LocalMap ()
initLocalMap (Map ss _ ms) = G.gmap ini $ G.undir $ mkUGraph (Prelude.map deSite ss) []
  where
    ini (ins, n, _, outs) = ( ins
                            , n, if n `elem` ms then Mine (IntSet.insert n IntSet.empty) else Normal
                            , outs)
    
{- |
直前ターンの参加者の手のリスト PrevMoves で AvailableRiversGr を更新
取られた辺(River)を消す
-}

updateAvailableRivers :: P.PrevMoves a -> AvailableRiversGr () -> AvailableRiversGr ()
updateAvailableRivers (P.PrevMoves (P.Moves ms) _) gr
  = foldr update gr ms
    where
      update m g = case m of
        P.MvClaim _ s t -> delEdges [(s,t),(t,s)] g
        P.MvPass _      -> g
        _               -> g

{- |
直前のターンの参加者 PrevMoves で LocalMapTable を更新
-}

updateLocalMapTable :: P.PrevMoves a -> LocalMapTable () -> LocalMapTable ()
updateLocalMapTable (P.PrevMoves (P.Moves ms) _) tbl
  = foldr update tbl ms
    where
      update mv@(P.MvClaim p _ _)
        = IntMap.update (return . updateLocalMap mv) p
      update _ = Prelude.id

{- |
Move で LocalMap を更新
-}

updateLocalMap :: P.Move -> LocalMap () -> LocalMap ()
updateLocalMap (P.MvClaim _ s t) mp
  = case (G.lab mp s, G.lab mp t) of
      (Just Normal, Just Normal) -> mp1
      (Just Normal, Just (Mine xs))
        -> case G.bfs s mp of
             ns -> G.gmap (\ ctx@(p,v,_,q) -> if v `L.notElem` (s:ns) then ctx else (p,v,Pseudo xs,q)) mp1
      (Just Normal, Just (Pseudo xs))
        -> case G.bfs s mp of
             ns -> G.gmap (\ ctx@(p,v,_,q) -> if v `L.notElem` (s:ns) then ctx else (p,v,Pseudo xs,q)) mp1
      (Just (Mine xs), Just Normal)
        -> case G.bfs t mp of
             ns -> G.gmap (\ ctx@(p,v,_,q) -> if v `L.notElem` (t:ns) then ctx else (p,v,Pseudo xs,q)) mp1
      (Just (Mine xs), Just (Mine ys))
        -> if xs == ys then mp1
           else case G.bfs s mp ++ G.bfs t mp of
             ns -> G.gmap (\ ctx@(p,v,l,q) -> if v `L.notElem` (s:t:ns) then ctx
                            else case l of
                              Mine _   -> (p,v,Mine (IntSet.union xs ys),q)
                              Pseudo _ -> (p,v,Pseudo (IntSet.union xs ys),q)
                              _         -> error $ "updateLocalMap: unexpected label: " ++ show l) mp1
      (Just (Mine xs), Just (Pseudo ys))
        -> if xs == ys then mp1
           else case G.bfs s mp ++ G.bfs t mp of
             ns -> G.gmap (\ ctx@(p,v,l,q) -> if v `L.notElem` (s:t:ns) then ctx
                            else case l of
                              Mine _   -> (p,v,Mine (IntSet.union xs ys),q)
                              Pseudo _ -> (p,v,Pseudo (IntSet.union xs ys),q)
                              _        -> error $ "updateLocalMap: unexpected label: " ++ show l) mp1
      (Just (Pseudo xs), Just Normal)
        -> case G.bfs t mp of
             ns -> G.gmap (\ ctx@(p,v,_,q) -> if v `L.notElem` (t:ns) then ctx else (p,v,Pseudo xs,q)) mp1
      (Just (Pseudo xs), Just (Mine ys))
        -> if xs == ys then mp1
           else case G.bfs s mp ++ G.bfs t mp of
             ns -> G.gmap (\ ctx@(p,v,l,q) -> if v `L.notElem` (s:t:ns) then ctx
                            else case l of
                              Mine _   -> (p,v,Mine (IntSet.union xs ys),q)
                              Pseudo _ -> (p,v,Pseudo (IntSet.union xs ys),q)
                              _         -> error $ "updateLocalMap: unexpected label: " ++ show l) mp1
      (Just (Pseudo xs), Just (Pseudo ys))
        -> if xs == ys then mp1
           else case G.bfs s mp ++ G.bfs t mp of
             ns -> G.gmap (\ ctx@(p,v,l,q) -> if v `L.notElem` (s:t:ns) then ctx
                            else case l of
                              Mine _   -> (p,v,Mine (IntSet.union xs ys),q)
                              Pseudo _ -> (p,v,Pseudo (IntSet.union xs ys),q)
                              _        -> error $ "updateLocalMap: unexpected label: " ++ show l) mp1
      _ -> error "updateLocalMap: Site is not in Map"
    where
      mp1 = G.insEdges [(s,t,()),(t,s,())] mp
updateLocalMap _ mp = mp
