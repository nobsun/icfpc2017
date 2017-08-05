{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Proxy
import OnlinePlay
import qualified Punter.Pass as PassPunter
import qualified Punter.ClaimAny as AnyPunter
import System.IO
import System.Environment

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [name, port] <- getArgs
  case name of
    "pass" -> runPunterOnline (Proxy :: Proxy PassPunter.Punter) port
    "any" -> runPunterOnline (Proxy :: Proxy AnyPunter.Punter) port
    _ -> error "unknown punter algorithm"
