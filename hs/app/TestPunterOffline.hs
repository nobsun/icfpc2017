{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Proxy
import OfflinePlay
import qualified Punter.Pass as PassPunter
import qualified Punter.ClaimAny as AnyPunter
import System.IO
import System.Environment

main :: IO ()
main = do
  [name, port] <- getArgs
  case name of
    "pass" -> runPunterOffline (Proxy :: Proxy PassPunter.Punter)
    "any" -> runPunterOffline (Proxy :: Proxy AnyPunter.Punter)
    _ -> error "unknown punter algorithm"
