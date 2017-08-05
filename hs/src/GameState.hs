{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GameState where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.Graph.Inductive.Basic
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree

import Protocol

data GameState = GameState
  { gs_punter  :: Int
  , gs_punters :: Int
  , gs_mines :: [Node]
  , gs_map   :: Gr SiteLabel RiverLabel
  , gs_submaps :: [Gr SiteLabel RiverLabel]
  } deriving (Generic, Show)

type SiteLabel = ()
type RiverLabel = ()


initGameState :: Setup -> GameState
initGameState (Setup spunter spunters smap setting) = GameState
  { gs_punter = spunter
  , gs_punters = spunters
  , gs_mines = mineList smap
  , gs_map = inigraph smap
  , gs_submaps = Prelude.replicate spunters G.empty
  }

inigraph :: Map -> Gr SiteLabel RiverLabel
inigraph (Map ss rs ms) = mkGraph (Prelude.map (lnode . deSite) ss) (Prelude.map (ledge () . deRiver) rs)

mineList :: Map -> [Node]
mineList (Map _ _ ms) = ms

deSite :: Site -> SiteId
deSite (Site i) = i

deRiver :: River -> (SiteId, SiteId)
deRiver (River s t) = (s, t)

lnode :: SiteId -> LNode SiteLabel
lnode i = (i,())

ledge :: RiverLabel -> (SiteId, SiteId) -> LEdge RiverLabel
ledge lab (s,t) = (s,t,lab)

instance ToJSON GameState where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = Prelude.drop 3 })
instance FromJSON GameState where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = ("gs_" ++) })

instance (ToJSON a, ToJSON b) => ToJSON (Gr a b) where
  toJSON g = object ["nodes" .= nodes g, "edges" .= edges g]
instance (FromJSON a, FromJSON b) => FromJSON (Gr a b) where
  parseJSON (Object v) = mkGraph <$> v .: "nodes" <*> v .: "edges"
  parseJSON _          = return G.empty
