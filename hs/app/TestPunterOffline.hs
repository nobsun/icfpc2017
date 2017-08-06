{-# OPTIONS_GHC -Wall #-}

import Data.Proxy
import OfflinePlay
import qualified Punter.Pass as PassPunter
import qualified Punter.ClaimAny as AnyPunter
import qualified Punter.ClaimGreedy as GreedyPunter
import qualified Punter.MaxDegree as MaxDegree
import qualified Punter.Alternate as Alternate
import System.Environment


usage :: IO ()
usage =
  putStr $ unlines [ "Usage: TestPunterOffline {pass|any|greedy|max-degree}", "" ]

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
    "max-degree" -> runPunterOffline (Proxy :: Proxy MaxDegree.Punter)
    "greedy2" -> runPunterOffline (Proxy :: Proxy (Alternate.Alternate MaxDegree.Punter GreedyPunter.Punter))
    _  -> usage *> error "unknown punter algorithm"
