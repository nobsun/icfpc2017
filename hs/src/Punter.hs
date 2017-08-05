{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Punter where

import qualified Data.Aeson as J
import Data.Maybe
import qualified Protocol as P

class (J.ToJSON a, J.FromJSON a) => Punter a where
  setup :: P.Setup -> P.Ready a
  play  :: P.PrevMoves a -> P.MyMove a

-- ------------------------------------------------------------------------

data Dummy = Dummy P.PunterId

instance J.ToJSON Dummy where
  toJSON (Dummy pid) = J.toJSON pid

instance J.FromJSON Dummy where
  parseJSON v = Dummy <$> J.parseJSON v

instance Punter.Punter Dummy where
  setup s =
    P.ReadyOn
    { P.ready   = P.punter (s :: P.Setup)
    , P.state   = Just $ P.GState $ Dummy (P.punter (s :: P.Setup))
    , P.futures = Nothing
    }
  play prevMoves =
    P.MyMove
    { P.move  =
        P.MvPass
        { P.pass =
            case fromJust (P.state (prevMoves :: P.PrevMoves Dummy)) of
              P.GState (Dummy punter) -> P.Punter punter
        }
    , P.state = P.state (prevMoves :: P.PrevMoves Dummy)
    }

-- ------------------------------------------------------------------------
