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
  play P.PrevMoves{ P.state = Just st1, P.move = moves } =
    P.MyMove
    { P.move  = move
    , P.state = Just st3
    }
    where
      punterId = P.punter (setupInfo :: P.Setup)

      st2@Punter{ setupInfo = setupInfo, availableRivers = availableRivers1, myRivers = myRivers1 } = update moves st1

      (move, st3) =
        case Set.toList availableRivers1 of
          [] ->
            ( P.MvPass punterId
            , st2
            )
          r@(s,t):_ ->
            ( P.MvClaim
              { P.punter = punterId
              , P.source = s
              , P.target = t
              }
            , st2
              { availableRivers = Set.delete r availableRivers1
              , myRivers = Set.insert r myRivers1
              }
            )

-- 他のプレイヤーの打った手による状態更新
update :: P.Moves -> Punter -> Punter
update P.Moves{ P.moves = moves } p1@Punter{ availableRivers = availableRivers1 } =
  p1
  { availableRivers = availableRivers1 `Set.difference` Set.fromList [e | P.MvClaim _punter' source target <- moves, e <- [(source,target), (target,source)]]
  }
