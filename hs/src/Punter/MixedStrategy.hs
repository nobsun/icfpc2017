{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Punter.MixedStrategy where

import Control.DeepSeq
import qualified Data.Aeson as J
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.IntMap.Lazy as IntMap
import GHC.Generics

import qualified CommonState as CS
import qualified Protocol as P
import Punter
import NormTypes
import DistanceTable (DistanceTable, mkDistanceTable, Futures)
import Strategy.Score (greedyDiffs)
import Strategy.Degree (higherDegrees)
import Strategy.Neighbor (neighborRivers)


data Punter
  = Punter
  { setupInfo :: P.Setup
  , distanceTable :: DistanceTable
  , futuresTable :: DistanceTable.Futures
  , movePool :: CS.MovePool
  }
  deriving (Generic, NFData)


instance J.ToJSON Punter
instance J.FromJSON Punter

instance Punter.IsPunter Punter where
  setup s =
    P.ReadyOn
    { P.ready   = P.setupPunter s
    , P.state   = Just $
        Punter
        { setupInfo = s
        , distanceTable = mkDistanceTable m
        , futuresTable = futures
        , movePool = CS.empty (P.punters s) m (P.settings' s)
        }
    , P.futures =
        if IntMap.null futures
        then Nothing
        else Just [P.Future mine site | (mine,site) <- IntMap.toList futures]
    }
    where
      m = P.map s
      futures = IntMap.empty

  applyMoves (P.Moves moves) p1@Punter{ setupInfo = si, movePool = movePool1 } =
    p1
    { movePool = CS.applyMoves2 (P.setupPunter si)  moves movePool1
    }

  chooseMoveSimple Punter{ setupInfo = si, distanceTable = distTbl, futuresTable = futures, movePool = pool } =
      case greedyDiffs distTbl futures siteClasses ars of
        []                           ->  P.MvPass punterId
        (crs1, _score):_  ->  case neighborRivers distTbl siteClasses $ Set.fromList crs1 of
          []                         ->  maybe (P.MvPass punterId) claimNR $ listToMaybe crs1  -- should not be happen
          ([], _d):_                 ->  maybe (P.MvPass punterId) claimNR $ listToMaybe crs1  -- should not be happen
          ([(r, _n)], _d):_          ->  claimNR r
          (crs2@((r, n):_), _d):_
            | n > 1                  ->  claimNR r
            | otherwise    ->  case higherDegrees mines ars siteClasses (Set.fromList $ map fst crs2) of
                []                   ->  claimNR r
                (crs3, _degree):_    ->  maybe (claimNR r) claimNR $ listToMaybe crs3
    where
      punterId = P.setupPunter si
      ars = CS.unclaimedRivers pool
      siteClasses = CS.reachabilityOf pool punterId
      mines = P.mines (P.map si)
      claimNR nr | let (s, t) = deNRiver nr = P.MvClaim punterId s t
