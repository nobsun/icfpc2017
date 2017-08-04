module Main where

import Data.Char (isDigit)
import Data.List (isPrefixOf, tails)
import Debug.Trace (traceShow)
import Network.Socket
import Control.Exception (bracket)
import Control.Monad (forever)
import System.IO
import System.Environment


main :: IO ()
main = do
    [port] <- getArgs
    withSocketsDo $ do
      addrinfos <- getAddrInfo Nothing (Just "punter.inf.ed.ac.uk") (Just port)
      let addr = head addrinfos
      sock <- socket (addrFamily addr) Stream defaultProtocol
      connect sock (addrAddress addr)
      bracket
        (socketToHandle sock ReadWriteMode)
        (hClose)
        (\h -> do
          hSetBuffering h LineBuffering
          output h $ "{\"me\":\"oga\"}"
          info <- input h
          let pid = getPunterId info
          output h $ "{\"ready\":"++show pid++"}"
          loop h pid
        )
  where
    loop h pid = do
      input h
      output h $ "{\"pass\":{\"punter\":"++show pid++"}"
      loop h pid


output :: Handle -> String -> IO ()
output h str = do
  putStrLn ("-> " ++ str)
  hPutStrLn h (show (length str)++":"++str)

input :: Handle -> IO String
input h = do
  str <- hGetLine h
  putStrLn ("<- " ++ str)
  return str

getPunterId :: String -> Int
getPunterId = read.takeWhile isDigit.drop 8.head.dropWhile(not.isPrefixOf"punter\":").tails
