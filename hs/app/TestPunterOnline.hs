{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Proxy
import OnlinePlay
import qualified Punter.Pass as PassPunter
import qualified Punter.ClaimAny as AnyPunter
import qualified Punter.ClaimGreedy as GreedyPunter
import qualified Punter.MaxDegree as MaxDegree
import qualified Punter.Alternate as Alternate
import qualified Punter.STF as STF
import System.IO
import System.Environment

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [name, port] <- getArgs
  case name of
    "pass" -> runPunterOnline (Proxy :: Proxy PassPunter.Punter) port
    "any" -> runPunterOnline (Proxy :: Proxy AnyPunter.Punter) port
    "greedy" -> runPunterOnline (Proxy :: Proxy GreedyPunter.Punter) port
    "max-degree" -> runPunterOnline (Proxy :: Proxy MaxDegree.Punter) port
    "greedy2" -> runPunterOnline (Proxy :: Proxy (Alternate.Alternate MaxDegree.Punter GreedyPunter.Punter)) port
    "stf" -> runPunterOnline (Proxy :: Proxy STF.Punter) port
    _ -> error "unknown punter algorithm"
