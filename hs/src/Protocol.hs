{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Protocol where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import Data.Text

-- Handshake

type Name = Text

data HandshakePunter = HandshakePunter { me :: Name } deriving (Generic, Show)
data HandshakeServer = HandshakeServer { you :: Name } deriving (Generic, Show)

instance ToJSON HandshakePunter
instance ToJSON HandshakeServer
instance FromJSON HandshakePunter
instance FromJSON HandshakeServer

-- Setup

data Setup = Setup
  { punter :: PunterId
  , punters :: Int
  , map :: Map
  , setting :: Maybe Settings
  } deriving (Generic, Show)

data Ready = ReadyOn
  { ready :: PunterId
  , state :: Maybe GState
  , futures :: Maybe Futures
  } deriving (Generic, Show)

instance ToJSON Setup where
  toJSON = genericToJSON (defaultOptions { omitNothingFields = True })
instance ToJSON Ready where
  toJSON = genericToJSON (defaultOptions { omitNothingFields = True })

instance FromJSON Setup
instance FromJSON Ready

--  Gameplay

data PrevMoves = PrevMoves
  { move :: Moves
  , state :: Maybe GState
  } deriving (Generic, Show)

data MyMove = MyMove
  { move :: Move
  , state :: Maybe GState
  } deriving (Generic, Show)

instance ToJSON PrevMoves where
  toJSON = genericToJSON (defaultOptions { omitNothingFields = True })
instance FromJSON PrevMoves

instance ToJSON MyMove where
  toJSON = genericToJSON (defaultOptions { omitNothingFields = True })
instance FromJSON MyMove

-- Scoring

data Scoring = Scoring { stop :: Stop } deriving (Generic, Show)

instance ToJSON Scoring
instance FromJSON Scoring

----

type PunterId = Int

data Map = Map
  { sites :: [Site]
  , rivers :: [River]
  , mines :: [SiteId]
  } deriving (Generic, Show)

instance ToJSON Map
instance FromJSON Map

data GState = GState { state :: () } deriving (Generic, Show)

instance ToJSON GState
instance FromJSON GState

data Settings = Settings { futures :: Bool } deriving (Generic, Show)
type Futures = [Future]

data Future = Future { source :: SiteId
                     , target :: SiteId
                     } deriving (Generic, Show)

instance ToJSON Settings
instance FromJSON Settings

instance ToJSON Future
instance FromJSON Future

data Site = Site { id :: SiteId } deriving (Generic, Show)
data River = River { source :: SiteId, target :: SiteId } deriving (Generic, Show)

instance ToJSON Site
instance ToJSON River
instance FromJSON Site
instance FromJSON River

type SiteId = Int

data Moves = Moves
  { moves :: [Move] } deriving (Generic, Show)

instance ToJSON Moves
instance FromJSON Moves

data Move = MvClaim { claim :: Claim }
          | MvPass { pass :: Punter}
          deriving (Generic, Show)

instance ToJSON Move where
  toJSON = genericToJSON (defaultOptions { sumEncoding = UntaggedValue  })
  
instance FromJSON Move where
  parseJSON = genericParseJSON (defaultOptions { sumEncoding = UntaggedValue  })


data Punter = Punter { punter :: PunterId } deriving (Generic, Show)

instance ToJSON Punter
instance FromJSON Punter

data Claim = Claim
  { punter :: PunterId
  , source :: SiteId
  , target :: SiteId
  } deriving (Generic, Show)

instance ToJSON Claim
instance FromJSON Claim

data Stop = Stop { moves :: [Move]
                 , scores :: [Score]
                 } deriving (Generic, Show)

instance ToJSON Stop
instance FromJSON Stop

data Score = Score { punter :: PunterId
                   , score :: Int
                   } deriving (Generic, Show)

instance ToJSON Score
instance FromJSON Score

