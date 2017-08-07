{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Punter.ClaimGreedy where

import Data.Bool (bool)
import Control.Monad (forM_)
import qualified Data.Aeson as J
import qualified Data.IntMap.Lazy as IM
import Data.List (maximumBy)
import Data.Ord
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import GHC.Generics

import qualified CommonState as CS
import qualified Protocol as P
import Punter
import NormTypes
import qualified UnionFind as UF
import qualified ScoreTable as ScoreTable

data Punter
  = Punter
  { setupInfo :: P.Setup
  , scoreTable :: ScoreTable.ScoreTable
  , availableRivers :: Set NRiver
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
        , scoreTable = ScoreTable.mkScoreTable m
        , availableRivers = Set.fromList [toNRiver' s' t' | P.River s' t' <- P.rivers m]
        , movePool = CS.empty
        }
    , P.futures = Nothing
    }
    where
      m = P.map s

  applyMoves (P.Moves moves) p1@Punter{ availableRivers = availableRivers1, movePool = movePool1 } =
    p1
    { availableRivers = availableRivers1 \\ Set.fromList [ toNRiver' s t | P.MvClaim _punter' s t <- moves ]
    , movePool = CS.applyMoves moves movePool1
    }

  chooseMoveSimple Punter{ setupInfo = si, scoreTable = tbl, availableRivers = ars, movePool = pool } =
    if Set.null ars then
      P.MvPass punterId
    else
      let (s,t) = deNRiver $ fst $ maximumBy (comparing snd) scores
      in P.MvClaim punterId s t
    where
      punterId = P.setupPunter si
      siteClasses = CS.reachabilityOf pool punterId
      scores = [(r, ScoreTable.computeScore tbl (UF.unify siteClasses s t)) | r <- Set.toList ars, let (s,t) = deNRiver r]

  logger p@Punter { setupInfo = P.Setup { punter = myid}, scoreTable = tbl, movePool = pool } = do
    -- scores
    forM_ (IM.toList $ CS.scores pool tbl) $ \(pid, s) -> do
      putStrLn $ (bool " "  "> " $ pid == myid) ++ "punter: " ++ show pid ++ " score: " ++ show s
    return p
