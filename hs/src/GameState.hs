{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GameState where

import GHC.Generics
import Data.Aeson
import Data.Text
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree

import Protocol

data GameState = GameState
  { punter :: Int
  , punters :: Int
  , graph :: Gr SiteLabel RiverLabel
  , mines :: [Node]
  } deriving (Generic, Show)

type SiteLabel = Bool
type RiverLabel = PunterId

nonClaimed :: RiverLabel
nonClaimed = -1

mine :: SiteLabel
mine = True

initGameState :: Setup -> GameState
initGameState (Setup spunter spunters smap setting) = GameState
  { punter = spunter
  , punters = spunters
  , graph = inigraph smap
  , mines = mineList smap
  }

inigraph :: Map -> Gr SiteLabel RiverLabel
inigraph (Map ss rs ms) = mkGraph (Prelude.map (lnode ms . deSite) ss) (Prelude.map (ledge nonClaimed . deRiver) rs)

mineList :: Map -> [Node]
mineList (Map _ _ ms) = ms

deSite :: Site -> SiteId
deSite (Site i) = i

deRiver :: River -> (SiteId, SiteId)
deRiver (River s t) = (s, t)

lnode :: [SiteId] -> SiteId -> LNode SiteLabel
lnode ms si
  | si `elem` ms = (si, True)
  | otherwise    = (si, False)

ledge :: RiverLabel -> (SiteId, SiteId) -> LEdge RiverLabel
ledge lab (s,t) = (s,t,lab)

instance ToJSON GameState
instance FromJSON GameState

instance (ToJSON a, ToJSON b) => ToJSON (Gr a b) where
  toJSON g = object ["labNodes" .= labNodes g, "labEdges" .= labEdges g]
instance (FromJSON a, FromJSON b) => FromJSON (Gr a b) where
  parseJSON (Object v) = mkGraph <$> v .: "labNodes" <*> v .: "labEdges"
  parseJSON _          = return G.empty
