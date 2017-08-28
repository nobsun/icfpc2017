{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module CommonState
  ( MovePool
  , empty
  , applyMove
  , applyMoves
  , applyMoves2
  , filterMoves
  , riversOf
  , reachabilityOf
  , scoreOf
  , scores
  , unclaimedRivers
  , optionableRivers
  , optionsOf
  ) where

import Control.DeepSeq
import Control.Monad
import Data.List
import qualified Data.Aeson as J
import GHC.Generics
import qualified Data.IntMap.Lazy as IM
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Protocol as P
import qualified UnionFind as UF
import qualified DistanceTable as DistanceTable
import NormTypes


data MovePool
  = MovePool
  { unclaimedRivers :: Set NRiver
  , optionableRivers :: Set NRiver
  , pool :: IM.IntMap Entry
  , numOptions :: IM.IntMap Int
  , pastMoves :: IM.IntMap [P.Move]
  , splurges :: Bool
  } deriving (Show, Generic, NFData)

type Entry = (Set NRiver, UF.Table)

emptyEntry :: Entry
emptyEntry = (Set.empty, UF.emptyTable)

empty :: Int -> P.Map -> P.Settings -> MovePool
empty numPunters m settings =
  MovePool
  { unclaimedRivers = Set.fromList $ map toNRiver $ P.rivers m
  , optionableRivers = Set.empty
  , pool = IM.fromList [(p, emptyEntry) | p <- [0..numPunters-1]]
  , numOptions = IM.fromList [(p, initialNumOptions) | p <- [0..numPunters-1]]
  , pastMoves  = IM.fromList [(p, []) | p <- [0..numPunters-1]]
  , splurges = P.splurges settings
  }
  where
    numMines = length $ P.mines m
    initialNumOptions =
      case settings of
        P.Settings{ P.options = True } -> numMines
        _ -> 0

applyMoves :: [P.Move] -> MovePool -> MovePool
applyMoves moves pl = foldl' (flip applyMove) pl moves

-- 最初にPrevMovesを受け取るときには、まだ指していないPunterの手に対してPassが入ってしまっているので、それを取り除く
-- TODO: 良い名前
applyMoves2 :: P.PunterId -> [P.Move] -> MovePool -> MovePool
applyMoves2 pid moves pl = applyMoves (filterMoves pid pl moves) pl

applyMove :: P.Move -> MovePool -> MovePool
applyMove move pl =
  case applyMoveChecked move pl of
    Nothing -> pass (P.punter (move :: P.Move)) pl
    Just pl' -> pl'

applyMoveChecked :: P.Move -> MovePool -> Maybe MovePool
applyMoveChecked (P.MvPass p) pl = Just $ pass p pl
applyMoveChecked move@(P.MvClaim p s t) orig@MovePool{ unclaimedRivers = urs, optionableRivers = ors, pool = pl } = do
  let r = toNRiver' s t
  guard $ r `Set.member` urs
  let (rs, e) = IM.findWithDefault emptyEntry p pl
  return $
    orig
    { unclaimedRivers = Set.delete r urs
    , optionableRivers = Set.insert r ors
    , pool = IM.insert p (Set.insert r rs, UF.unify e s t) pl
    , pastMoves = IM.adjust (move :) p (pastMoves orig)
    }
applyMoveChecked move@(P.MvOption p s t) orig@MovePool{ optionableRivers = ors, pool = pl, numOptions = nOpts } = do
  let r = toNRiver' s t
      n = IM.findWithDefault 0 p nOpts
  guard $ r `Set.member` ors
  guard $ n > 0
  let (rs, e) = IM.findWithDefault emptyEntry p pl
  return $
    orig
    { optionableRivers = Set.delete r ors
    , pool = IM.insert p (Set.insert r rs, UF.unify e s t) pl
    , numOptions = IM.insert p (n-1) nOpts
    , pastMoves = IM.adjust (move :) p (pastMoves orig)
    }
applyMoveChecked move@(P.MvSplurge p ss) orig@MovePool{ unclaimedRivers = urs, optionableRivers = ors, pool = pl, numOptions = nOpts } = do
  let (rs, e) = IM.findWithDefault emptyEntry p pl
      n = IM.findWithDefault 0 p nOpts
      rs2 = zip ss (tail ss)
      rs2' = Set.fromList $ map (uncurry toNRiver') rs2
      optionRivers = rs2' Set.\\ urs
  guard $ splurges orig
  guard $ all isPass $ take (length rs2 - 1) $ pastMoves orig IM.! p
  guard $ optionRivers `Set.isSubsetOf` ors
  guard $ n >= Set.size optionRivers
  return $
    orig
    { unclaimedRivers = urs Set.\\ rs2'
    , optionableRivers = ors Set.\\ optionRivers
    , pool = IM.insert p (rs `Set.union` rs2', UF.unifyN e rs2) pl
    , numOptions = IM.insert p (n - Set.size optionRivers) nOpts
    , pastMoves = IM.adjust (move :) p (pastMoves orig)
    }

-- 最初にPrevMovesを受け取るときには、まだ指していないPunterの手に対してPassが入ってしまっているので、それを取り除く
-- 最初のターンにタイムアウトしてしまって、次のターンにまとめてくるときにも余計なパスは入るのだろうか? (要確認)
filterMoves :: P.PunterId -> MovePool -> [P.Move] -> [P.Move]
filterMoves pid pl xs
  | isFirstTurn = xs Data.List.\\ [P.MvPass pid' | pid' <- [pid..numPunters-1]] -- 1個ずつ取り除く
  | otherwise   = xs
  where
    numPunters = fst $ IM.findMax (pool pl)
    isFirstTurn =
      all (Set.null . fst) (IM.elems (pool pl)) &&
      and [P.MvPass pid' `elem` xs | pid' <- [pid..numPunters-1]]

pass :: P.PunterId -> MovePool -> MovePool
pass p orig = orig{ pastMoves = IM.adjust (P.MvPass p :) p (pastMoves orig) }

isPass :: P.Move -> Bool
isPass (P.MvPass _) = True
isPass _ = False

instance J.ToJSON MovePool
instance J.FromJSON MovePool

riversOf :: MovePool -> P.PunterId -> Set NRiver
riversOf MovePool{ pool = pl } pid = fst $ IM.findWithDefault emptyEntry pid pl

reachabilityOf :: MovePool -> P.PunterId -> UF.Table
reachabilityOf MovePool{ pool = pl } pid = snd $ IM.findWithDefault emptyEntry pid pl

scoreOf :: MovePool -> DistanceTable.DistanceTable -> IM.IntMap DistanceTable.Futures -> P.PunterId -> DistanceTable.Score
scoreOf MovePool{ pool = pl } tbl futures pid = DistanceTable.computeScore tbl (IM.findWithDefault IM.empty pid futures) (snd (IM.findWithDefault emptyEntry pid pl))

scores :: MovePool -> DistanceTable.DistanceTable -> IM.IntMap DistanceTable.Futures -> IM.IntMap DistanceTable.Score
scores MovePool{ pool = pl } tbl futures = IM.mapWithKey (\pid (_, equiv) -> DistanceTable.computeScore tbl (IM.findWithDefault IM.empty pid futures) equiv) pl

optionsOf :: MovePool -> P.PunterId -> Int
optionsOf MovePool{ numOptions = numOpts } p = numOpts IM.! p
