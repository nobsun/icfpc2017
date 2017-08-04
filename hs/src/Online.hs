{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Online where

import GHC.Generics
import Data.Aeson
import Data.Text

import InternalJSON

-- Online: Handshake

type Name = Text

data HandshakePunter = HandshakePunter { me :: Name } deriving (Generic, Show)
data HandshakeServer = HandshakeServer { you :: Name } deriving (Generic, Show)

instance ToJSON HandshakePunter
instance ToJSON HandshakeServer
instance FromJSON HandshakePunter
instance FromJSON HandshakeServer

-- Online: Setup

{- -
data Setup = Setup
  { punter :: PunterId
  , punters :: Int
  , map :: Map
  } deriving (Generic, Show)

data Ready = ReadyOn
  { ready :: PunterId } deriving (Generic, Show)
-- -}

data Setup = Setup
  { punter :: PunterId
  , punters :: Int
  , map :: Map
  , setting :: Settings
  } deriving (Generic, Show)

data Ready = ReadyOn
  { ready :: PunterId
  , futures :: Futures
  } deriving (Generic, Show)

instance ToJSON Setup
instance ToJSON Ready
instance FromJSON Setup
instance FromJSON Ready

-- Online: Gameplay

data PrevMoves = PrevMoves
  { move :: Moves } deriving (Generic, Show)
type MyMove = Move

instance ToJSON PrevMoves
instance FromJSON PrevMoves

-- Online: Scoring

data Scoring = Scoring { stop :: Stop } deriving (Generic, Show)

instance ToJSON Scoring
instance FromJSON Scoring

