{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Punter.MaxDegree where

import Control.Monad
import qualified Data.Aeson as J
import Data.List (maximumBy)
import Data.Ord
-- import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import GHC.Generics

import qualified CommonState as CS
import qualified Protocol as P
import Punter
import NormTypes
import qualified UnionFind as UF

data Punter
  = Punter
  { setupInfo :: P.Setup
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

  chooseMoveSimple Punter{ setupInfo = si, availableRivers = ars, movePool = pool } =
    if Set.null ars then
      P.MvPass punterId
    else
      let (s,t) = deNRiver $ fst $ maximumBy (comparing snd) scores
      in P.MvClaim punterId s t
    where
      punterId = P.setupPunter si
      siteClasses = CS.reachabilityOf pool punterId

      scores = [(r, f r (UF.unify siteClasses s t)) | r <- Set.toList ars, let (s,t) = deNRiver r]
      f r siteClasses2 = length $ do
        let ars2 = Set.delete r ars
        mine <- P.mines (P.map si)
        let sites = IntSet.fromList $ UF.classToList $ UF.getClass siteClasses2 mine
        r2  <- Set.toList ars2
        let (s,t) = deNRiver r2
        -- エッジの一方の頂点だけがsitesに含まれる
        guard $ (s `IntSet.member`) sites /= (t `IntSet.member`) sites
        return ()
