{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module InternalJSON where

import GHC.Generics
import Data.Aeson
import Data.Text

{- -
type Name = String

data HandshakePunter = HandshakePunter { me :: Name } deriving (Generic, Show)
data HandshakeServer = HandshakeServer { you :: Name } deriving (Generic, Show)

instance ToJSON HandshakePunter
instance ToJSON HandshakeServer
instance FromJSON HandshakePunter
instance FromJSON HandshakeServer

data Setup = Setup { punter :: PunterId, punters :: Int, map :: Map } deriving (Generic, Show)
data Ready = Ready { ready :: PunterId } deriving (Generic, Show)

instance ToJSON Setup
instance ToJSON Ready
instance FromJSON Setup
instance FromJSON Ready
-- comment out by @nobsun -}

type PunterId = Int

data Map = Map
  { sites :: [Site]
  , rivers :: [River]
  , mines :: [SiteId]
  } deriving (Generic, Show)

instance ToJSON Map
instance FromJSON Map

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

instance ToJSON Move
instance FromJSON Move

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
