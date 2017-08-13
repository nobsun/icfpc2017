{-# OPTIONS_GHC -Wall #-}
module Main where

import Data.Default.Class
import OnlinePlay
import qualified Punters as Punters
import System.IO
import System.Environment

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [name, port] <- getArgs
  case Punters.withPunter name (runPunterOnline def{ OnlinePlay.optServiceName = port }) of
    Just act -> act
    Nothing -> error "unknown punter algorithm"
