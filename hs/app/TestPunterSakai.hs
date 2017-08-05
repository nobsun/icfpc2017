{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit)
import qualified Data.Aeson as J
import Data.Maybe
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import qualified Network.Socket as N
import Control.Exception (bracket)
import System.IO
import System.Environment

import qualified Protocol as P
import Punter  
import Punter.Pass as PassPunter

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  [port] <- getArgs
  test port

test :: N.ServiceName -> IO ()
test port = do
  N.withSocketsDo $ do
    addrinfos <- N.getAddrInfo Nothing (Just "punter.inf.ed.ac.uk") (Just port)
    let addr = head addrinfos
    sock <- N.socket (N.addrFamily addr) N.Stream N.defaultProtocol
    N.connect sock (N.addrAddress addr)
    bracket
      (N.socketToHandle sock ReadWriteMode)
      (hClose)
      (\h -> do
        hSetBuffering h NoBuffering
        runPunterOnline "sampou" (Proxy :: Proxy PassPunter.PassPunter) h)

runPunterOnline :: forall a. Punter.Punter a => T.Text -> Proxy a -> Handle -> IO ()
runPunterOnline name _ h = do
  send h $ P.HandshakePunter{ P.me=name }
  (_::P.HandshakeServer) <- recv h
  setupInfo <- recv h
  let (ready :: P.Ready a) = Punter.setup setupInfo
  let Just s = P.state (ready :: P.Ready a)
  send h $ (ready{ P.state = Nothing } :: P.Ready ())
  let loop :: P.GState a -> IO ()
      loop s = do
        (v :: J.Value) <- recv h
        case J.fromJSON v of
          J.Success (moves :: P.PrevMoves a) -> do
            let move = Punter.play $ moves{ P.state = Just s }
            send h $ (move{ P.state = Nothing } :: P.MyMove ())
            loop $ fromJust $ P.state (move :: P.MyMove a)
          J.Error _ -> do
            case J.fromJSON v of
              J.Success (_ :: P.Scoring) -> return ()
              J.Error _ -> error ("unknown messsage: " ++ show v)
  loop s

send :: J.ToJSON a => Handle -> a -> IO ()
send h x = do
  let json = J.encode x
  B.hPutStrLn stderr ("-> " <> json)
  hFlush stderr
  B.hPutStr h $ B.pack (show (B.length json)) <> ":" <> json

recv :: J.FromJSON a => Handle -> IO a
recv h = do
  len <- getLength []
  s <- B.hGet h len
  B.hPutStrLn stderr $ "<- " <> s
  case J.decode s of
    Nothing -> error ("failed to parse " ++ show s)
    Just a -> return a
  where
    getLength cs = do
      c <- hGetChar h
      if isDigit c then getLength (c:cs) else return (read (reverse cs))
