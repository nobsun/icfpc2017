{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Punter.Connector where

import Data.Bool (bool)
import Control.Monad
import qualified Data.Aeson as J
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.IntMap.Lazy as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (maximumBy)
import Data.Ord
import qualified Data.Set as Set
import GHC.Generics

import Dijkstra
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
        , movePool = CS.empty m
        }
    , P.futures = Nothing
    }
    where
      m = P.map s

  applyMoves (P.Moves moves) p1@Punter{ movePool = movePool1 } =
    p1
    { movePool = CS.applyMoves moves movePool1
    }

  chooseMoveSimple Punter{ setupInfo = si, scoreTable = tbl, movePool = pool } =
    if null candidates then
      P.MvPass punterId
    else
      let (s,t) = deNRiver $ fst $ maximumBy (comparing snd) candidates
      in P.MvClaim punterId s t
    where
      punterId = P.setupPunter si
      m = P.map si
      ars = CS.unclaimedRivers pool
      mrs = CS.riversOf pool punterId
      equiv = CS.reachabilityOf pool punterId

      -- これ自体はMineではないので注意
      mineReprs :: IntSet
      mineReprs = IntSet.fromList [UF.getRepr equiv mine | mine <- P.mines m]

      siteReprs :: IntSet
      siteReprs = IntSet.fromList [UF.getRepr equiv site | P.Site site <- P.sites m]

      candidates :: [(NRiver, Double)]
      candidates = do
        let currScore = CS.scoreOf pool tbl punterId
        s1 <- IntSet.toList mineReprs
        let st = dijkstra g [s1]
        s2 <- IntSet.toList siteReprs
        guard $ s1 /= s2
        case HashMap.lookup s2 st of
          Nothing -> mzero
          Just (n, _) -> do
            let p = path st s2
                futureScore = ScoreTable.computeScore tbl (UF.unifyN equiv (map deNRiver p))
                nextRiver = head [r | r <- p, r `Set.member` CS.unclaimedRivers pool]
                reward = fromIntegral (futureScore - currScore) / fromIntegral n -- 将来報酬の割引をすると良い
            return (nextRiver, reward)

      g :: HashMap P.SiteId [(P.SiteId, Int, NRiver)]
      g = HashMap.fromListWith (++) $
            [e | r <- Set.toList ars, let (s,t) = deNRiver r, e <- [(s, [(t,1,r)]), (t, [(s,1,r)])]] ++
            [e | r <- Set.toList mrs, let (s,t) = deNRiver r, e <- [(s, [(t,0,r)]), (t, [(s,0,r)])]]

      path st t = reverse $ loop t
        where
          loop s =
            case HashMap.lookup s st of
              Just (_, Nothing) -> []
              Just (_, Just (s', r)) -> r : loop s'
              Nothing -> undefined

  logger Punter{ setupInfo = P.Setup { punter = myid}, scoreTable = tbl, movePool = pool } = do
    -- scores
    forM_ (IM.toList $ CS.scores pool tbl) $ \(pid, s) -> do
      writeLog $ (bool "  "  "> " $ pid == myid) ++ "punter: " ++ show pid ++ " score: " ++ show s