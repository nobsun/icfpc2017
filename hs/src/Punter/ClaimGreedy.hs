{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Punter.ClaimGreedy where

import qualified Data.Aeson as J
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import GHC.Generics

import Dijkstra
import qualified Protocol as P
import Punter

data Punter
  = Punter
  { setupInfo :: P.Setup
  , scoreTable :: IntMap (IntMap Integer)
  , availableRivers :: Set (Int,Int)
  , myRivers :: Set (Int,Int)
  , mySites :: Set P.SiteId
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
        , scoreTable = scores
        , availableRivers = Set.fromList [(s',t') | P.River s' t' <- P.rivers (P.map s)]
        , myRivers = Set.empty
        , mySites = Set.empty
        }
    , P.futures = Nothing
    }
    where
      m = P.map s

      mines :: IntSet
      mines = IntSet.fromList $ P.mines m

      g :: HashMap P.SiteId [(P.SiteId, Integer, ())]
      g = HashMap.unionWith (++)
            (HashMap.fromList [(site, []) | P.Site site <- P.sites m])
            (HashMap.fromListWith (++) [e | P.River src tgt <- P.rivers m, e <- [(src, [(tgt, 1, ())]), (tgt, [(src, 1, ())])]])

      scores :: IntMap (IntMap Integer)
      scores = IntMap.fromList [(mine, IntMap.fromList [(site,d*d) | (site, (d, _)) <- HashMap.toList (dijkstra g [mine]), site `IntSet.notMember` mines ]) | mine <- P.mines m]

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
update P.Moves{ P.moves = moves } p1@Punter{ availableRivers = availableRivers1, myRivers = myRivers1, mySites = mySites1 } =
  p1
  { availableRivers = availableRivers1 \\ Set.fromList [e | P.MvClaim _punter' s t <- moves, e <- [(s,t), (t,s)]]
  , myRivers = myRivers1 `Set.union` Set.fromList [(s,t) | P.MvClaim _punter' s t <- moves]
  , mySites = mySites1 `Set.union` Set.fromList [e | P.MvClaim _punter' s t <- moves, e <- [s, t]]
  }

choice :: Punter -> Maybe (Int, Int)
choice Punter { availableRivers = ars } = listToMaybe $ Set.toList ars
