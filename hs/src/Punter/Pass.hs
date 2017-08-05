{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Punter.Pass where

import qualified Data.Aeson as J
import Data.Maybe
import qualified Protocol as P
import Punter

data PassPunter = PassPunter P.PunterId

instance J.ToJSON PassPunter where
  toJSON (PassPunter pid) = J.toJSON pid

instance J.FromJSON PassPunter where
  parseJSON v = PassPunter <$> J.parseJSON v

instance Punter.IsPunter PassPunter where
  setup s =
    P.ReadyOn
    { P.ready   = P.punter (s :: P.Setup)
    , P.state   = Just $ P.GState $ PassPunter (P.punter (s :: P.Setup))
    , P.futures = Nothing
    }
  play prevMoves =
    P.MyMove
    { P.move  =
        P.MvPass
        { P.pass =
            case fromJust (P.state (prevMoves :: P.PrevMoves PassPunter)) of
              P.GState (PassPunter punter) -> P.Punter punter
        }
    , P.state = P.state (prevMoves :: P.PrevMoves PassPunter)
    }

