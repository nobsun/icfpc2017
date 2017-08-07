{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Proxy
import OfflinePlay
import qualified Punter.Connector as Connector

main :: IO ()
main = runPunterOffline (Proxy :: Proxy Connector.Punter)
