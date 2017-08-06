{-# OPTIONS_GHC -Wall #-}

import Data.List (intercalate)
import OfflinePlay
import qualified Punters as Punters
import System.Environment


usage :: IO ()
usage =
  putStr $ unlines [ "Usage: TestPunterOffline {" ++ intercalate "|" Punters.names ++ "}", "" ]

main :: IO ()
main = do
  args <- getArgs
  name  <-  case args of
    []      ->  usage *> error "punter algorithm name is required"
    name:_  ->  return name

  case Punters.withPunter name runPunterOffline of
    Just act -> act
    Nothing -> usage *> error "unknown punter algorithm"
