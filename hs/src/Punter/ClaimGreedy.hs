{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Punter.ClaimGreedy where

import qualified Data.Aeson as J
import Data.List (maximumBy, foldl')
import Data.Ord
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import GHC.Generics

import CommonState as CS
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
  , siteClasses :: UF.Table
  , movePool :: MovePool
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
        , scoreTable = ScoreTable.mkScoreTable m
        , availableRivers = Set.fromList [toNRiver' s' t' | P.River s' t' <- P.rivers m]
        , siteClasses = UF.emptyTable
        , movePool = CS.empty
        }
    , P.futures = Nothing
    }
    where
      m = P.map s

  applyMoves (P.Moves moves) p1@Punter{ setupInfo = si, availableRivers = availableRivers1, siteClasses = siteClasses1, movePool = movePool1 } =
    p1
    { availableRivers = availableRivers1 \\ Set.fromList [ toNRiver' s t | P.MvClaim _punter' s t <- moves ]
    , siteClasses = siteClasses2
    , movePool = movePool2
    }
    where
      punterId = P.punter (si :: P.Setup)

      siteClasses2 = foldl' f siteClasses1 moves
        where
          f tbl (P.MvClaim punter' s t)
            | punter' == punterId = UF.unify tbl s t
          f tbl _ = tbl

      movePool2 = CS.applyMoves moves movePool1
      

  chooseMoveSimple Punter{ setupInfo = si, scoreTable = tbl, availableRivers = ars, siteClasses = siteClasses1 } =
    if Set.null ars then
      P.MvPass punterId 
    else
      let (s,t) = deNRiver $ fst $ maximumBy (comparing snd) scores
      in P.MvClaim punterId s t
    where
      punterId = P.punter (si :: P.Setup)
  
      scores = [(r, ScoreTable.computeScore tbl (UF.unify siteClasses1 s t)) | r <- Set.toList ars, let (s,t) = deNRiver r]
