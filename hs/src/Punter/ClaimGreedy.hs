{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Punter.ClaimGreedy where

import Data.Bool (bool)
import Control.Monad (forM_)
import qualified Data.Aeson as J
import qualified Data.IntMap.Lazy as IM (IntMap, mapWithKey, toList)
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
    { P.ready   = P.setupPunter s
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
      siteClasses2 = foldl' f siteClasses1 moves
        where
          f tbl (P.MvClaim punter' s t)
            | punter' == P.setupPunter si  = UF.unify tbl s t
            | otherwise                    = tbl
          f tbl (P.MvPass {})              = tbl

      movePool2 = CS.applyMoves moves movePool1


  chooseMoveSimple Punter{ setupInfo = si, scoreTable = tbl, availableRivers = ars, siteClasses = siteClasses1 } =
    if Set.null ars then
      P.MvPass punterId
    else
      let (s,t) = deNRiver $ fst $ maximumBy (comparing snd) scores
      in P.MvClaim punterId s t
    where
      punterId = P.setupPunter si

      scores = [(r, ScoreTable.computeScore tbl (UF.unify siteClasses1 s t)) | r <- Set.toList ars, let (s,t) = deNRiver r]

  logger p@Punter { setupInfo = P.Setup { punter = myid}, scoreTable = tbl, movePool = pool } = do
    -- scores
    forM_ (IM.toList $ scores pool) $ \(pid, s) -> do
      putStrLn $ (bool "  "  "> " $ pid == myid) ++ "punter: " ++ show pid ++ " score: " ++ show s
    return p
    where
      scores :: MovePool -> IM.IntMap Integer
      scores MovePool{ CS.pool = pl} = IM.mapWithKey (\_ t -> ScoreTable.computeScore tbl t) pl
