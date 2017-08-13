{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Default.Class
import Data.Proxy
import OfflinePlay
import qualified Punter.Connector as Connector

main :: IO ()
main = runPunterOffline def (Proxy :: Proxy Connector.Punter)
