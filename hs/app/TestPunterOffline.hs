{-# OPTIONS_GHC -Wall #-}

import Data.Proxy
import OfflinePlay
import qualified Punter.Pass as PassPunter
import qualified Punter.ClaimAny as AnyPunter
import qualified Punter.ClaimGreedy as GreedyPunter
import System.Environment


usage :: IO ()
usage =
  putStr $ unlines [ "Usage: TestPunterOffline {pass|any|greedy}", "" ]

main :: IO ()
main = do
  args <- getArgs
  name  <-  case args of
    []      ->  usage *> error "punter algorithm name is required"
    name:_  ->  return name

  case name of
    "pass"    -> runPunterOffline (Proxy :: Proxy PassPunter.Punter)
    "any"     -> runPunterOffline (Proxy :: Proxy AnyPunter.Punter)
    "greedy"  -> runPunterOffline (Proxy :: Proxy GreedyPunter.Punter)
    _  -> usage *> error "unknown punter algorithm"
