{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Proxy
import OfflinePlay
import qualified Punter.Pass as PassPunter
import qualified Punter.ClaimAny as AnyPunter
import qualified Punter.ClaimGreedy as GreedyPunter
import System.Environment

main :: IO ()
main = do
  [name, _port] <- getArgs
  case name of
    "pass" -> runPunterOffline (Proxy :: Proxy PassPunter.Punter)
    "any" -> runPunterOffline (Proxy :: Proxy AnyPunter.Punter)
    "greedy" -> runPunterOffline (Proxy :: Proxy GreedyPunter.Punter)
    _ -> error "unknown punter algorithm"
