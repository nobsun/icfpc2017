{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Punter.MixedStrategy where

import qualified Data.Aeson as J
import Data.Maybe
import qualified Data.Set as Set
import GHC.Generics

import qualified CommonState as CS
import qualified Protocol as P
import Punter
import NormTypes
import ScoreTable (ScoreTable, mkScoreTable)
import DistanceTable (DistanceTable, mkDistanceTable)
import Strategy.Score (greedyDiffs)
import Strategy.Degree (higherDegrees)
import Strategy.Neighbor (neighborRivers)


data Punter
  = Punter
  { setupInfo :: P.Setup
  , scoreTable :: ScoreTable
  , distanceTable :: DistanceTable
  , movePool :: CS.MovePool
  }
  deriving (Generic)


instance J.ToJSON Punter
instance J.FromJSON Punter

instance Punter.IsPunter Punter where
  setup s =
    P.ReadyOn
    { P.ready   = P.setupPunter s
    , P.state   = Just $
        Punter
        { setupInfo = s
        , scoreTable = mkScoreTable m
        , distanceTable = mkDistanceTable m
        , movePool = CS.empty (P.punters s) m (P.settings' s)
        }
    , P.futures = Nothing
    }
    where
      m = P.map s

  applyMoves (P.Moves moves) p1@Punter{ movePool = movePool1 } =
    p1
    { movePool = CS.applyMoves moves movePool1
    }

  chooseMoveSimple Punter{ setupInfo = si, scoreTable = scoreTbl, distanceTable = distTbl, movePool = pool } =
      case greedyDiffs scoreTbl siteClasses ars of
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
