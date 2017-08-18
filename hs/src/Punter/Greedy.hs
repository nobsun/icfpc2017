{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Punter.Greedy where

import Data.Bool (bool)
import Control.DeepSeq
import Control.Monad (forM_)
import qualified Data.Aeson as J
import qualified Data.IntMap.Lazy as IM
import Data.List (maximumBy)
import Data.Ord
import qualified Data.Set as Set
import GHC.Generics

import qualified CommonState as CS
import qualified Protocol as P
import Punter
import NormTypes
import qualified DistanceTable as DistanceTable

data Punter
  = Punter
  { setupInfo :: P.Setup
  , distTable :: DistanceTable.DistanceTable
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
        , distTable = DistanceTable.mkDistanceTable m
        , futuresTable = futures
        , movePool = CS.empty (P.punters s) m (P.settings' s)
        }
    , P.futures =
        if IM.null futures
        then Nothing
        else Just [P.Future mine site | (mine,site) <- IM.toList futures]
    }
    where
      m = P.map s
      futures = IM.empty

  applyMoves (P.Moves moves) p1@Punter{ movePool = movePool1 } =
    p1
    { movePool = CS.applyMoves moves movePool1
    }

  chooseMoveSimple Punter{ setupInfo = si, distTable = tbl, futuresTable = futures, movePool = pool } =
    if Set.null ars then
      P.MvPass punterId
    else
      let (s,t) = deNRiver $ fst $ maximumBy (comparing snd) rewards
      in P.MvClaim punterId s t
    where
      punterId = P.setupPunter si
      ars = CS.unclaimedRivers pool
      siteClasses = CS.reachabilityOf pool punterId
      rewards = [(r, DistanceTable.reward tbl futures siteClasses r) | r <- Set.toList ars]

  logger Punter{ setupInfo = P.Setup { punter = myid}, distTable = tbl, futuresTable = futures, movePool = pool } = do
    forM_ (IM.toList $ CS.scores pool tbl (IM.singleton myid futures)) $ \(pid, s) -> do
      writeLog $ (bool "  "  "> " $ pid == myid) ++ "punter: " ++ show pid ++ " score: " ++ show s
