{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module GameState where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Graph.Inductive.Graph as G
import Data.Graph.Inductive.PatriciaTree

import Protocol
import NormTypes

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
initGameState (Setup spunter spunters smap _setting) = GameState
  { gs_punter = spunter
  , gs_punters = spunters
  , gs_mines = mineList smap
  , gs_map = inigraph smap
  , gs_submaps = Prelude.replicate spunters G.empty
  }

inigraph :: Map -> Gr SiteLabel RiverLabel
inigraph (Map ss rs _) = mkGraph (Prelude.map (lnode . deSite) ss) (Prelude.map (ledge () . deNRiver . toNRiver) rs)

mineList :: Map -> [Node]
mineList (Map _ _ ms) = ms

deSite :: Site -> SiteId
deSite (Site i) = i

lnode :: SiteId -> LNode SiteLabel
lnode i = (i,())

ledge :: RiverLabel -> (SiteId, SiteId) -> LEdge RiverLabel
ledge lb (s,t) = (s,t,lb)

instance ToJSON GameState where
  toJSON = genericToJSON (defaultOptions { fieldLabelModifier = Prelude.drop 3 })
instance FromJSON GameState where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = ("gs_" ++) })

instance (ToJSON a, ToJSON b) => ToJSON (Gr a b) where
  toJSON g = object ["nodes" .= nodes g, "edges" .= edges g]
instance (FromJSON a, FromJSON b) => FromJSON (Gr a b) where
  parseJSON (Object v) = mkGraph <$> v .: "nodes" <*> v .: "edges"
  parseJSON _          = return G.empty
