{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Online where

import GHC.Generics
import Data.Aeson
import Data.Text

import InternalJSON

-- Online: Handshake

data HandshakePS = HandshakePS { me :: Text } deriving (Generic, Show)
data HandshakeSP = HandshakeSP { you :: Text } deriving (Generic, Show)

instance ToJSON HandshakePS
instance ToJSON HandshakeSP
instance FromJSON HandshakePS
instance FromJSON HandshakeSP

-- Online: Setup

data SetupSP = SetupSP
  { punter :: PunterId
  , punters :: Int
  , map :: Map
  } deriving (Generic, Show)

data SetupPS = SetupPSOn
  { ready :: PunterId } deriving (Generic, Show)

instance ToJSON SetupSP
instance ToJSON SetupPS
instance FromJSON SetupSP
instance FromJSON SetupPS

-- Online: Gameplay

data GameplaySP = GameplaySP
  { move :: Moves } deriving (Generic, Show)
type GameplayPS = Move

-- Online: Scoring

data ScoringSP = ScoringSP { stop :: Stop } deriving (Generic, Show)

instance ToJSON ScoringSP
instance FromJSON ScoringSP

