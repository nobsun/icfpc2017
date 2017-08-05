module Punter where

import qualified Data.Aeson as J
import qualified Protocol as P

class (J.ToJSON a, J.FromJSON a) => Punter a where
  setup :: P.Setup -> P.Ready a
  play  :: P.PrevMoves a -> P.MyMove a
