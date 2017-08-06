{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Punter.MaxDegree where

import Control.Monad
import qualified Data.Aeson as J
import Data.List (maximumBy, foldl')
import Data.Ord
-- import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import GHC.Generics

import qualified Protocol as P
import Punter
import NormTypes
import qualified UnionFind as UF

data Punter
  = Punter
  { setupInfo :: P.Setup
  , availableRivers :: Set NRiver
  , siteClasses :: UF.Table
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
        , availableRivers = Set.fromList [toNRiver' s' t' | P.River s' t' <- P.rivers m]
        , siteClasses = UF.emptyTable
        }
    , P.futures = Nothing
    }
    where
      m = P.map s

  applyMoves (P.Moves moves) p1@Punter{ setupInfo = si, availableRivers = availableRivers1, siteClasses = siteClasses1 } =
    p1
    { availableRivers = availableRivers1 \\ Set.fromList [ toNRiver' s t | P.MvClaim _punter' s t <- moves ]
    , siteClasses = siteClasses2
    }
    where
      siteClasses2 = foldl' f siteClasses1 moves
        where
          f tbl (P.MvClaim punter' s t)
            | punter' == P.setupPunter si  = UF.unify tbl s t
          f tbl (P.MvPass {})              = tbl

  chooseMoveSimple Punter{ setupInfo = si, availableRivers = ars, siteClasses = siteClasses1 } =
    if Set.null ars then
      P.MvPass punterId
    else
      let (s,t) = deNRiver $ fst $ maximumBy (comparing snd) scores
      in P.MvClaim punterId s t
    where
      punterId = P.setupPunter si

      scores = [(r, f r (UF.unify siteClasses1 s t)) | r <- Set.toList ars, let (s,t) = deNRiver r]
      f r siteClasses2 = length $ do
        let ars2 = Set.delete r ars
        mine <- P.mines (P.map si)
        let sites = IntSet.fromList $ UF.classToList $ UF.getClass siteClasses2 mine
        r2  <- Set.toList ars2
        let (s,t) = deNRiver r2
        -- エッジの一方の頂点だけがsitesに含まれる
        guard $ (s `IntSet.member`) sites /= (t `IntSet.member`) sites
        return ()
