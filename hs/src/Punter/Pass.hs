{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Punter.Pass where

import qualified Data.Aeson as J
import qualified Protocol as P
import Punter

data Punter = Punter P.PunterId

instance J.ToJSON Punter where
  toJSON (Punter pid) = J.toJSON pid

instance J.FromJSON Punter where
  parseJSON v = Punter <$> J.parseJSON v

instance Punter.IsPunter Punter where
  setup s =
    P.ReadyOn
    { P.ready   = P.punter (s :: P.Setup)
    , P.state   = Just $ Punter (P.punter (s :: P.Setup))
    , P.futures = Nothing
    }

  applyMoves _ = id

  chooseMoveSimple (Punter pid) = P.MvPass pid
