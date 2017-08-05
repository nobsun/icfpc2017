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

  play P.PrevMoves{ P.state = Just st1, P.move = moves } =
    P.MyMove
    { P.move  = move
    , P.state = Just st2
    }
    where
      punterId = P.punter (si :: P.Setup)

      p'@Punter{ setupInfo = si, availableRivers = availableRivers1, myRivers = myRivers1, mySites = mySites1 } = update moves st1

      (move, st2) =
        case choice p' of
          Nothing ->
            ( P.MvPass punterId
            , st1
            )
          Just r -> case deNRiver r of
            (s, t) ->
              ( P.MvClaim
                { P.punter = punterId
                , P.source = s
                , P.target = t
                }
              , st1
                { availableRivers = Set.delete r availableRivers1
                , myRivers = myRivers2
                , mySites = mySites2
                }
              )
            where
              myRivers2 = Set.insert r myRivers1
              mySites2 = fmap (IntSet.fromList . HashMap.keys . dijkstra g . IntSet.toList) mySites1
                where
                  g :: HashMap P.SiteId [(P.SiteId, Integer, ())]
                  g = HashMap.fromListWith (++) [e | (src, tgt) <- fmap deNRiver $ Set.toList $ myRivers2, e <- [(src, [(tgt, 0, ())]), (tgt, [(src, 0, ())])]]

-- 他のプレイヤーの打った手による状態更新
update :: P.Moves -> Punter -> Punter
update P.Moves{ P.moves = moves } p1@Punter{ availableRivers = availableRivers1 } =
  p1
  { availableRivers = availableRivers1 \\ Set.fromList [toNRiver' s t | P.MvClaim _punter' s t <- moves]
  }

choice :: Punter -> Maybe NRiver
choice Punter { setupInfo = si, scoreTable = tbl, availableRivers = ars, myRivers = myRivers1, mySites = mySites1 }
  | Set.null ars = Nothing 
  | otherwise = Just $ fst $ maximumBy (comparing snd) scores
  where
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
