{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Punter.Longest where

import qualified Data.Aeson as J
import Data.List (sortBy)
import Data.Ord
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import GHC.Generics

import qualified CommonState as CS
import qualified Protocol as P
import Punter
import NormTypes
import Dijkstra
import qualified UnionFind as UF
import qualified ScoreTable as ScoreTable

data Punter
  = Punter
  { setupInfo :: P.Setup
  , scoreTable :: ScoreTable.ScoreTable
  , movePool :: CS.MovePool
  , targets :: [(P.SiteId, P.SiteId)]
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
        , scoreTable = sc
        , movePool = CS.empty m
        , targets = map fst $ sortBy (flip (comparing snd)) [((mine,site),w) | (mine,sites) <- IntMap.toList sc, (site,w) <- IntMap.toList sites]
        }
    , P.futures = Nothing
    }
    where
      m = P.map s
      sc = ScoreTable.mkScoreTable m

  applyMoves (P.Moves moves) p1@Punter{ movePool = movePool1 } =
    p1
    { movePool = CS.applyMoves moves movePool1
    }

  chooseMoveSimple = undefined

  chooseMove p@Punter{ setupInfo = si, movePool = pool, targets = tgs }
    | Set.null ars = P.MyMove (P.MvPass punterId) (Just p)
    | Just r <- mr, (s,t) <- deNRiver r = P.MyMove (P.MvClaim punterId s t) (Just p{ targets = tgs2 })
    | otherwise = P.MyMove (P.MvPass punterId) (Just p)
    where
      punterId = P.setupPunter si
      siteClasses1 = CS.reachabilityOf pool punterId
      ars = CS.unclaimedRivers pool
      mrs = CS.riversOf pool punterId

      (mr, tgs2) = f tgs

      f :: [(P.SiteId,P.SiteId)] -> (Maybe NRiver, [(P.SiteId,P.SiteId)])
      f [] = (Nothing, [])
      f xxs@((mine,site) : xs)
        | not (site `HashMap.member` stree) = f xs
        | not (r `Set.member` ars) = f xs
        | otherwise = (Just r, xxs)
        where
          mineRepr = UF.getRepr siteClasses1 mine
          stree = spanningTrees IntMap.! mine
          r = loop site
            where
              loop site' =
                 case stree HashMap.! site'  of
                   (_, Nothing) -> undefined
                   (_, Just (parentSite, r2)) ->
                     if UF.getRepr siteClasses1 parentSite == mineRepr
                     then r2
                     else loop parentSite

      spanningTrees :: IntMap (HashMap P.SiteId (Int, Maybe (P.SiteId, NRiver)))
      spanningTrees = IntMap.fromList [(mine, dijkstra g [mine]) | mine <- P.mines (P.map si)]

      g :: HashMap P.SiteId [(P.SiteId, Int, NRiver)]
      g = HashMap.fromListWith (++) $
            [e | r <- Set.toList ars, let (s,t) = deNRiver r, e <- [(s, [(t,1,r)]), (t, [(s,1,r)])]] ++
            [e | r <- Set.toList mrs, let (s,t) = deNRiver r, e <- [(s, [(t,0,r)]), (t, [(s,0,r)])]]
      
