{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Proxy
import OfflinePlay
import Punter.Any
-- import System.IO
-- import System.Environment

main :: IO ()
main = runPunterOffline (Proxy :: Proxy Punter)
