{-# OPTIONS_GHC -Wall #-}
module Main where

import OnlinePlay
import qualified Punters as Punters
import System.IO
import System.Environment

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [name, port] <- getArgs
  case Punters.withPunter name (\p -> runPunterOnline p port) of
    Just act -> act
    Nothing -> error "unknown punter algorithm"
