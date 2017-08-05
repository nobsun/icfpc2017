{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Punter.ClaimAny where

import qualified Data.Aeson as J
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import qualified Protocol as P
import Punter

data Punter
  = Punter
  { setupInfo :: P.Setup
  , availableRivers :: Set (Int,Int)
  , myRivers :: Set (Int,Int)
  }
  deriving (Generic)

instance J.ToJSON Punter
instance J.FromJSON Punter

instance Punter.IsPunter Punter where
  setup s =
    P.ReadyOn
    { P.ready   = P.punter (s :: P.Setup)
    , P.state   = Just $
        Punter
        { setupInfo = s
        , availableRivers = Set.fromList [(s',t') | P.River s' t' <- P.rivers (P.map s)]
        , myRivers = Set.empty
        }
    , P.futures = Nothing
    }
  play P.PrevMoves{ P.state = Just st1, P.move = P.Moves{ P.moves = moves } } =
    P.MyMove
    { P.move  = move
    , P.state = Just st2
    }
    where
      Punter{ setupInfo = setupInfo, availableRivers = availableRivers1, myRivers = myRivers1 } = st1
      
      punterId = P.punter (setupInfo :: P.Setup)

      ars = availableRivers1 `Set.difference` Set.fromList [e | P.MvClaim _punter' source target <- moves, e <- [(source,target), (target,source)]]
      (move, availableRivers2, myRivers2) =
        case Set.toList ars of
          [] -> (P.MvPass punterId, ars, myRivers1)
          r@(s,t):_ ->
            ( P.MvClaim
              { P.punter = punterId
              , P.source = s
              , P.target = t
              }
            , Set.delete r ars
            , Set.insert r myRivers1
            )
      st2 =
        st1
        { availableRivers = availableRivers2
        , myRivers = myRivers2
        }

