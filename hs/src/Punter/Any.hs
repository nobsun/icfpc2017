{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Punter.Any where

import qualified Data.Aeson as J
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import GHC.Generics
import qualified CommonState as CS
import qualified Protocol as P
import Punter
import NormTypes (deNRiver)

data Punter
  = Punter
  { setupInfo :: P.Setup
  , movePool :: CS.MovePool
  }
  deriving (Generic, Show)

instance J.ToJSON Punter
instance J.FromJSON Punter

instance Punter.IsPunter Punter where
  setup s =
    P.ReadyOn
    { P.ready   = P.setupPunter s
    , P.state   = Just $
        Punter
        { setupInfo = s
        , movePool = CS.empty (P.punters s) (P.map s) (P.settings' s)
        }
    , P.futures = Nothing
    }

  applyMoves (P.Moves moves) p1@Punter{ movePool = movePool1 } =
    p1
    { movePool = CS.applyMoves moves movePool1
    }

  chooseMoveSimple Punter{ setupInfo = si, movePool = pool } =
    case listToMaybe $ Set.toList $ CS.unclaimedRivers pool of
      Nothing -> P.MvPass punterId
      Just r | (s,t) <- deNRiver r -> P.MvClaim punterId s t
    where
      punterId = P.setupPunter si
