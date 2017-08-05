{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Punter.ClaimGreedy where

import qualified Data.Aeson as J
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (maximumBy)
import Data.Ord
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Set (Set, (\\))
import qualified Data.Set as Set
import GHC.Generics

import Dijkstra
import qualified Protocol as P
import Punter
import NormTypes

data Punter
  = Punter
  { setupInfo :: P.Setup
  , scoreTable :: IntMap (IntMap Integer)
  , availableRivers :: Set NRiver
  , myRivers :: Set NRiver
  , mySites :: IntMap IntSet -- Map Mine (Set SiteId)
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
        , availableRivers = Set.fromList [toNRiver' s' t' | P.River s' t' <- P.rivers (P.map s)]
        , myRivers = Set.empty
        , mySites = IntMap.fromList [(mine, IntSet.singleton mine) | mine <- IntSet.toList mines]
        }
    , P.futures = Nothing
    }
    where
      m = P.map s

      mines :: IntSet
      mines = IntSet.fromList $ P.mines m

      g :: HashMap P.SiteId [(P.SiteId, Integer, ())]
      g = HashMap.fromListWith (++) [e | P.River src tgt <- P.rivers m, e <- [(src, [(tgt, 1, ())]), (tgt, [(src, 1, ())])]]

      scores :: IntMap (IntMap Integer)
      scores = IntMap.fromList [(mine, IntMap.fromList [(site,d*d) | (site, (d, _)) <- HashMap.toList (dijkstra g [mine])]) | mine <- P.mines m]

  applyMoves (P.Moves moves) p1@Punter{ setupInfo = si, availableRivers = availableRivers1, myRivers = myRivers1, mySites = mySites1 } =
    p1
    { availableRivers = availableRivers1 \\ Set.fromList [ toNRiver' s t | P.MvClaim _punter' s t <- moves ]
    , myRivers = myRivers2
    , mySites = mySites2
    }
    where
      punterId = P.punter (si :: P.Setup)

      myRivers2 = myRivers1 `Set.union` Set.fromList [ toNRiver' s t | P.MvClaim punter' s t <- moves, punter' == punterId ]

      mySites2 = fmap (IntSet.fromList . HashMap.keys . dijkstra g . IntSet.toList) mySites1
        where
          g :: HashMap P.SiteId [(P.SiteId, Integer, ())]
          g = HashMap.fromListWith (++) [e | (src, tgt) <- fmap deNRiver $ Set.toList $ myRivers2, e <- [(src, [(tgt, 0, ())]), (tgt, [(src, 0, ())])]]

  chooseMoveSimple Punter{ setupInfo = si, scoreTable = tbl, availableRivers = ars, myRivers = myRivers1, mySites = mySites1 } =
    if Set.null ars then
      P.MvPass punterId 
    else
      let (s,t) = deNRiver $ fst $ maximumBy (comparing snd) scores
      in P.MvClaim punterId s t
    where
      punterId = P.punter (si :: P.Setup)

      g0 :: HashMap P.SiteId [(P.SiteId, Integer, ())]
      g0 = HashMap.fromListWith (++) [e | (src, tgt) <- fmap deNRiver $ Set.toList $ myRivers1, e <- [(src, [(tgt, 0, ())]), (tgt, [(src, 0, ())])]]
  
      scores = [(r, score r) | r <- Set.toList ars]
  
      score :: NRiver -> Integer
      score r = sum [f r mine | mine <- P.mines (P.map si)]
        where
          f :: NRiver -> P.SiteId -> Integer
          f (deNRiver -> (s,t)) mine = sum [IntMap.findWithDefault 0 site tbl1 | site <- reachableSites]
            where
              g = HashMap.unionWith (++) g0 (HashMap.fromList [(s, [(t,0,())]), (t, [(s,0,())])])
              reachableSites = HashMap.keys (dijkstra g (IntSet.toList (mySites1 IntMap.! mine)))
              tbl1 = tbl IntMap.! mine
