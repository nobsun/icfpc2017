{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Punter.Alternate where

import Control.DeepSeq
import qualified Data.Aeson as J
import Data.Maybe
import GHC.Generics
import qualified Protocol as P
import Punter

data Alternate p1 p2 = Alternate Bool p1 p2
  deriving (Generic, NFData)

instance (J.ToJSON p1, J.ToJSON p2) => J.ToJSON (Alternate p1 p2)
instance (J.FromJSON p1, J.FromJSON p2) => J.FromJSON (Alternate p1 p2)

instance (Punter.IsPunter p1, Punter.IsPunter p2) => Punter.IsPunter (Alternate p1 p2)  where
  setup s =
    P.ReadyOn
    { P.ready   = P.setupPunter s
    , P.state   = Just $ Alternate True (fromJust $ P.readyState r1) (fromJust $ P.readyState r2)
    , P.futures = P.futures (r1 :: P.Ready p1)
    }
    where
      r1 = setup s
      r2 = setup s

  applyMoves moves (Alternate b p1 p2) =
    Alternate (not b) (applyMoves moves p1) (applyMoves moves p2)

  chooseMoveSimple (Alternate b p1 p2) =
    if b
    then chooseMoveSimple p1
    else chooseMoveSimple p2
