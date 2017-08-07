{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Punter.ClaimAny where

import qualified Data.Aeson as J
import Data.Maybe (listToMaybe)
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import GHC.Generics
import qualified Protocol as P
import Punter
import NormTypes (NRiver, toNRiver', deNRiver)

data Punter
  = Punter
  { setupInfo :: P.Setup
  , availableRivers :: Set NRiver
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
        , availableRivers = Set.fromList [ toNRiver' s' t' | P.River s' t' <- P.rivers (P.map s)]
        }
    , P.futures = Nothing
    }

  applyMoves (P.Moves moves) p1@Punter{ setupInfo = si, availableRivers = availableRivers1 } =
    p1
    { availableRivers = availableRivers1 \\ Set.fromList [ toNRiver' s t | P.MvClaim _punter' s t <- moves ]
    }

  chooseMoveSimple Punter{ setupInfo = si, availableRivers = availableRivers1 } =
    case listToMaybe $ Set.toList $ availableRivers1 of
      Nothing -> P.MvPass punterId
      Just r | (s,t) <- deNRiver r -> P.MvClaim punterId s t
    where
      punterId = P.setupPunter si
