{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Default.Class
import Data.Proxy
import OfflinePlay
import Punter.Any
-- import System.IO
-- import System.Environment

main :: IO ()
main = runPunterOffline def (Proxy :: Proxy Punter)
