{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Protocol where

import GHC.Generics
import Control.DeepSeq
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Text

-- Handshake

type Name = Text

data HandshakePunter = HandshakePunter { me :: Name } deriving (Generic, Show, NFData)
data HandshakeServer = HandshakeServer { you :: Name } deriving (Generic, Show, NFData)

instance ToJSON HandshakePunter
instance ToJSON HandshakeServer
instance FromJSON HandshakePunter
instance FromJSON HandshakeServer

-- Setup

data Setup = Setup
  { punter :: PunterId
  , punters :: Int
  , map :: Map
  , settings :: Maybe Settings
  } deriving (Generic, Show, NFData)

setupPunter :: Setup -> PunterId
setupPunter = punter

settings' :: Setup -> Settings
settings' = fromMaybe defaultSettings . settings

data Ready a = ReadyOn
  { ready :: PunterId
  , state :: Maybe a
  , futures :: Maybe Futures
  } deriving (Generic, Show, NFData)

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
  } deriving (Generic, Show, NFData)

data MyMove a = MyMove
  { move :: Move
  , state :: Maybe a
  } deriving (Generic, Show, NFData)

instance ToJSON a => ToJSON (PrevMoves a) where
  toJSON = genericToJSON (defaultOptions { omitNothingFields = True })
instance FromJSON a => FromJSON (PrevMoves a)

instance ToJSON a => ToJSON (MyMove a) where
  toJSON (MyMove m (Just s)) = Object $ HashMap.insert "state" (toJSON s) m'
    where
      Object m' = toJSON m
  toJSON (MyMove m Nothing)  = toJSON m
instance FromJSON a => FromJSON (MyMove a) where
  parseJSON = do
    withObject "MyMove" $ \obj -> do
      m <- parseJSON $ Object (HashMap.delete "state" obj)
      s <-
        case HashMap.lookup "state" obj of
          Nothing -> return Nothing
          Just st -> Just <$> parseJSON st
      return $ MyMove{ move  = m, state = s }

-- Scoring

data Scoring = Scoring { stop :: Stop } deriving (Generic, Show, NFData)

instance ToJSON Scoring
instance FromJSON Scoring

----

type PunterId = Int

data Map = Map
  { sites :: [Site]
  , rivers :: [River]
  , mines :: [SiteId]
  } deriving (Generic, Show, NFData)

instance ToJSON Map
instance FromJSON Map

data Settings = Settings { futures  :: Bool
                         , splurges :: Bool
                         , options  :: Bool
                         }
              deriving (Generic, Show, NFData)

instance ToJSON Settings where
  toJSON Settings{ futures=b1, splurges=b2, options=b3 } =
    object $ 
    [ "futures"  .= toJSON True | b1] ++
    [ "splurges" .= toJSON True | b2] ++
    [ "options"  .= toJSON True | b3]

instance FromJSON Settings where
  parseJSON = withObject "Settings" $ \obj ->
    Settings
    <$> (obj .:? "futures"  .!= False)
    <*> (obj .:? "splurges" .!= False)
    <*> (obj .:? "options"  .!= False)

defaultSettings :: Settings
defaultSettings =
    Settings
    { futures  = False
    , splurges = False
    , options  = False
    }

type Futures = [Future]

data Future = Future { source :: SiteId
                     , target :: SiteId
                     } deriving (Generic, Show, NFData)

instance ToJSON Future
instance FromJSON Future

data Site = Site { id :: SiteId } deriving (Generic, Show, Eq, Ord, NFData)
data River = River { source :: SiteId, target :: SiteId } deriving (Generic, Show, Eq, Ord, NFData)

instance ToJSON Site
instance ToJSON River
instance FromJSON Site
instance FromJSON River

type SiteId = Int

data Moves = Moves
  { moves :: [Move] } deriving (Generic, Show, NFData)

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
  | MvOption
    { punter :: PunterId
    , source :: SiteId
    , target :: SiteId
    }
  deriving (Generic, Show, NFData)

moveOptions :: Options
moveOptions =
  defaultOptions
  { constructorTagModifier = Prelude.map Data.Char.toLower . Prelude.drop 2
  , sumEncoding = ObjectWithSingleField
  }

instance ToJSON Move where
  toJSON = genericToJSON moveOptions

instance FromJSON Move where
  parseJSON = genericParseJSON moveOptions

data Stop = Stop { moves :: [Move]
                 , scores :: [Score]
                 } deriving (Generic, Show, NFData)

instance ToJSON Stop
instance FromJSON Stop

data Score = Score { punter :: PunterId
                   , score :: Int
                   } deriving (Generic, Show, NFData)

instance ToJSON Score
instance FromJSON Score

data Timeout = Timeout{ timeout :: Double }
  deriving (Generic, Show, NFData)

instance ToJSON Timeout
instance FromJSON Timeout

getMap :: FilePath -> IO (Maybe Map)
getMap path = BSL.readFile path >>= return.decode
