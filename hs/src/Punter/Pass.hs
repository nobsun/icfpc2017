{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Punter.Pass where

import Control.DeepSeq
import qualified Data.Aeson as J
import  GHC.Generics
import qualified Protocol as P
import Punter

data Punter = Punter P.PunterId
  deriving (Generic, NFData)

instance J.ToJSON Punter where
  toJSON (Punter pid) = J.toJSON pid

instance J.FromJSON Punter where
  parseJSON v = Punter <$> J.parseJSON v

instance Punter.IsPunter Punter where
  setup s =
    P.ReadyOn
    { P.ready   = P.setupPunter s
    , P.state   = Just $ Punter (P.setupPunter s)
    , P.futures = Nothing
    }

  applyMoves _ = id

  chooseMoveSimple (Punter pid) = P.MvPass pid
