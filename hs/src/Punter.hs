
module Punter where

import qualified Data.Aeson as J
import qualified Protocol as P

class (J.ToJSON a, J.FromJSON a) => IsPunter a where
  setup :: P.Setup -> P.Ready a
  play  :: P.PrevMoves a -> P.MyMove a

data OfflineStage
  = Setup
  | GamePlay Int
  | Scoring
  deriving (Eq, Ord, Show)

class IsPunter a => IsOfflinePunter a where
  offlineStage :: a -> OfflineStage
