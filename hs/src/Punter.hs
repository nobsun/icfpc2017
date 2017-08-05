{-# LANGUAGE DeriveGeneric #-}
module Punter where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Protocol as P

class (ToJSON a, FromJSON a) => IsPunter a where
  setup :: P.Setup -> P.Ready a
  play  :: P.PrevMoves a -> P.MyMove a

data OfflineStage
  = Setup
  | GamePlay Int
  | Scoring
  deriving (Eq, Ord, Show, Generic)

instance ToJSON OfflineStage
instance FromJSON OfflineStage

class IsPunter a => IsOfflinePunter a where
  offlineStage :: a -> OfflineStage
