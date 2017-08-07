{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Protocol where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
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

setupPunter :: Setup -> PunterId
setupPunter = punter

data Ready a = ReadyOn
  { ready :: PunterId
  , state :: Maybe a
  , futures :: Maybe Futures
  } deriving (Generic, Show)

readyState :: Ready a -> Maybe a
readyState = state

instance ToJSON Setup where
  toJSON = genericToJSON (defaultOptions { omitNothingFields = True })
instance ToJSON a => ToJSON (Ready a) where
  toJSON = genericToJSON (defaultOptions { omitNothingFields = True })

instance FromJSON Setup
instance FromJSON a => FromJSON (Ready a)

--  Gameplay

data PrevMoves a = PrevMoves
  { move :: Moves
  , state :: Maybe a
  } deriving (Generic, Show)

data MyMove a = MyMove
  { move :: Move
  , state :: Maybe a
  } deriving (Generic, Show)

instance ToJSON a => ToJSON (PrevMoves a) where
  toJSON = genericToJSON (defaultOptions { omitNothingFields = True })
instance FromJSON a => FromJSON (PrevMoves a)

instance ToJSON a => ToJSON (MyMove a) where
  toJSON (MyMove m (Just s)) = Object $ HashMap.insert "state" (toJSON s) m'
    where
      Object m' = toJSON m
  toJSON (MyMove m Nothing)  = toJSON m
instance FromJSON a => FromJSON (MyMove a)

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

data Settings = Settings { futures :: Bool } deriving (Generic, Show)
type Futures = [Future]

data Future = Future { source :: SiteId
                     , target :: SiteId
                     } deriving (Generic, Show)

instance ToJSON Settings
instance FromJSON Settings

instance ToJSON Future
instance FromJSON Future

data Site = Site { id :: SiteId } deriving (Generic, Show, Eq, Ord)
data River = River { source :: SiteId, target :: SiteId } deriving (Generic, Show, Eq, Ord)

instance ToJSON Site
instance ToJSON River
instance FromJSON Site
instance FromJSON River

type SiteId = Int

data Moves = Moves
  { moves :: [Move] } deriving (Generic, Show)

instance ToJSON Moves
instance FromJSON Moves

data Move
  = MvClaim
    { punter :: PunterId
    , source :: SiteId
    , target :: SiteId
    }
  | MvPass
    { punter :: PunterId
    }
  | MvSplurge
    { punter :: PunterId
    , route  :: [SiteId]
    }
  deriving (Generic, Show)

instance ToJSON Move where
  toJSON (MvClaim punterId src tgt) = object $
    [ "claim" .=
        object
        [ "punter" .= toJSON punterId
        , "source" .= toJSON src
        , "target" .= toJSON tgt
        ]
    ]
  toJSON (MvPass punterId) = object $
    [ "pass" .= object [ "punter" .= toJSON punterId ]
    ]
  toJSON (MvSplurge punterId route') = object $
    [ "splurge" .=
        object
        [ "punter" .= toJSON punterId
        , "route"  .= toJSON route'
        ]
    ]
  
instance FromJSON Move where
  parseJSON = do
    withObject "Move" $ \obj -> do
      let m = listToMaybe $ catMaybes
               [ do claim <- HashMap.lookup "claim" obj
                    return $ withObject "claim" (\obj2 -> MvClaim <$> (obj2 .: "punter") <*> (obj2 .: "source") <*> (obj2 .: "target")) claim
               , do pass <- HashMap.lookup "pass" obj
                    return $ withObject "pass" (\obj2 -> MvPass <$> (obj2 .: "punter")) pass
               , do splurge <- HashMap.lookup "splurge" obj
                    return $ withObject "splurge" (\obj2 -> MvSplurge <$> (obj2 .: "punter") <*> (obj2 .: "route")) splurge
               ]
      case m of
        Just act -> act
        Nothing -> fail $ show obj ++ " is not valid Move"

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

getMap :: FilePath -> IO (Maybe Map)
getMap path = BSL.readFile path >>= return.decode
