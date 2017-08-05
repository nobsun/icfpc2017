{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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

data Punter
  = Punter
  { setupInfo :: P.Setup
  , availableRivers :: Set (P.SiteId,P.SiteId)
  , myRivers :: Set (P.SiteId,P.SiteId)
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
    , P.state = Just st2
    }
    where
      punterId = P.punter (si :: P.Setup)

      p'@Punter{ setupInfo = si, availableRivers = availableRivers1, myRivers = myRivers1 } = update moves st1

      (move, st2) =
        case choice p' of
          Nothing ->
            ( P.MvPass punterId
            , st1
            )
          Just r@(s,t) ->
            ( P.MvClaim
              { P.punter = punterId
              , P.source = s
              , P.target = t
              }
            , st1
              { availableRivers = Set.delete r availableRivers1
              , myRivers = Set.insert r myRivers1
              }
            )

-- 他のプレイヤーの打った手による状態更新
update :: P.Moves -> Punter -> Punter
update P.Moves{ P.moves = moves } p1@Punter{ availableRivers = availableRivers1 } =
  p1
  { availableRivers = availableRivers1 \\ Set.fromList [e | P.MvClaim _punter' s t <- moves, e <- [(s,t), (t,s)]]
  }

choice :: Punter -> Maybe (P.SiteId, P.SiteId)
choice Punter { availableRivers = ars } = listToMaybe $ Set.toList ars
