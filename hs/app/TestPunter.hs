{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Lazy.Char8 as B (pack, unpack)
import Data.Char (isDigit)
import Data.Aeson (ToJSON, encode, decode)
import Data.List (isPrefixOf, tails)
-- import Debug.Trace (traceShow)
import Network.Socket
import Control.Exception (bracket)
import Control.Monad (replicateM, void)
import System.IO
import System.Environment


import Protocol

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    [port] <- getArgs
    withSocketsDo $ do
      addrinfos <- getAddrInfo Nothing (Just "punter.inf.ed.ac.uk") (Just port)
      let addr = head addrinfos
      sock <- socket (addrFamily addr) Stream defaultProtocol
      putStr "connecting ... "
      connect sock (addrAddress addr)
      putStrLn "done."
      bracket
        (socketToHandle sock ReadWriteMode)
        (hClose)
        (\h -> do
          hSetBuffering h NoBuffering
          output h (HandshakePunter{me="sampou"})
          void $ input h
          str <- input h
          let pid = getPunterId str -- workaround
          _setup <- maybe (fail "TestPunter: decode failure") return (decode (B.pack str) :: Maybe Setup)
          --let pid = (punter::Setup->Int) setup
          output h (ReadyOn{ready=pid, state=Nothing, futures=Nothing} :: Ready ())
          loop h pid
        )
  where
    loop h pid = do
      _token <- fmap B.pack (input h)
{- dummy -}
      output h (MvPass pid)
      loop h pid
{- not work
      case decode token :: Maybe PrevMoves of
         Just movest -> do
           output h (MvPass pid)
           loop h pid
         Nothing ->
           case decode token :: Maybe Stop of
             Just stop -> do
               print (scores stop)
             Nothing -> do
               putStrLn $ "fail to parse JSON: "++(B.unpack token)
               hFlush stdout
-}

output :: ToJSON a => Handle -> a -> IO ()
output h x = do
  let json = B.unpack (encode x)
  putStrLn ("-> " ++ json)
  hFlush stdout
  hPutStr h (show (length json)++":"++json)

input :: Handle -> IO String
input h = do
  len <- getLength []
  token <- replicateM len (hGetChar h)
  putStrLn ("<- " ++ token)
  hFlush stdout
  return token
  where
    getLength cs = do
      c <- hGetChar h
      if isDigit c then getLength (c:cs) else return (read (reverse cs))

getPunterId :: String -> Int
getPunterId = read.takeWhile isDigit.drop 8.head.dropWhile(not.isPrefixOf"punter\":").tails
