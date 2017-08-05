{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Punter.Pass where

import qualified Data.Aeson as J
import Data.Maybe
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
  play prevMoves =
    P.MyMove
    { P.move  =
        P.MvPass
        { P.pass =
            case fromJust (P.state (prevMoves :: P.PrevMoves Punter)) of
              Punter punter -> P.Punter punter
        }
    , P.state = P.state (prevMoves :: P.PrevMoves Punter)
    }
