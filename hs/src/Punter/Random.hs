{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Punter.Random where

import Control.DeepSeq
import qualified Data.Aeson as J
import qualified Data.Set as Set
import GHC.Generics
import qualified System.Random as Random
import qualified CommonState as CS
import qualified Protocol as P
import Punter
import NormTypes (deNRiver)

data Punter
  = Punter
  { setupInfo :: P.Setup
  , movePool :: CS.MovePool
  , gen :: String
  }
  deriving (Generic, NFData, Show)

instance J.ToJSON Punter
instance J.FromJSON Punter

-- instance NFData Punter where
--   rnf p = get p `seq` rnf (setupInfo p) `seq` rnf (movePool p)

instance Punter.IsPunter Punter where
  setup s =
    P.ReadyOn
    { P.ready   = P.setupPunter s
    , P.state   = Just $
        Punter
        { setupInfo = s
        , movePool = CS.empty (P.punters s) (P.map s) (P.settings' s)
        , gen = show $ Random.mkStdGen 123456
        }
    , P.futures = Nothing
    }

  applyMoves (P.Moves moves) p1@Punter{ setupInfo = si, movePool = movePool1, gen = gen1 } =
    p1
    { movePool = CS.applyMoves2 (P.setupPunter si) moves movePool1
    , gen = show . snd . Random.next . (read :: String -> Random.StdGen) $ gen1
    }

  chooseMoveSimple Punter{ setupInfo = si, movePool = pool, gen = gen1 }
    | Set.null rs = P.MvPass punterId
    | r `Set.member` CS.unclaimedRivers pool, (s,t) <- deNRiver r = P.MvClaim punterId s t
    | (s,t) <- deNRiver r = P.MvOption punterId s t
    where
      punterId = P.setupPunter si
      rs = CS.unclaimedRivers pool `Set.union` (if CS.optionsOf pool punterId > 0 then CS.optionableRivers pool else Set.empty)
      gen1' :: Random.StdGen
      gen1' = read gen1
      r = Set.elemAt (fst (Random.randomR (0, Set.size rs - 1) gen1')) rs
