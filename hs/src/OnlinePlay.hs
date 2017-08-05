{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module OnlinePlay where

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

import qualified Protocol as P
import Punter  
import qualified Punter.Pass as PassPunter

test :: String -> IO ()
test port = runPunterOnline (Proxy :: Proxy PassPunter.Punter) port

runPunterOnline :: Punter.IsPunter a => Proxy a -> N.ServiceName -> IO ()
runPunterOnline punter port = do
  N.withSocketsDo $ do
    addrinfos <- N.getAddrInfo Nothing (Just "punter.inf.ed.ac.uk") (Just port)
    let addr = head addrinfos
    sock <- N.socket (N.addrFamily addr) N.Stream N.defaultProtocol
    N.connect sock (N.addrAddress addr)
    bracket (N.socketToHandle sock ReadWriteMode) hClose $ \h -> do
      hSetBuffering h NoBuffering
      runPunterOnline' "sampou" punter h

runPunterOnline' :: forall a. Punter.IsPunter a => T.Text -> Proxy a -> Handle -> IO ()
runPunterOnline' name _ h = do
  send h $ P.HandshakePunter{ P.me=name }
  (_::P.HandshakeServer) <- recv h
  setupInfo <- recv h
  let (ready :: P.Ready a) = Punter.setup setupInfo
  let Just s = P.state (ready :: P.Ready a)
  send h $ (ready{ P.state = Nothing } :: P.Ready ())
  let loop :: a -> IO ()
      loop s' = do
        (v :: J.Value) <- recv h
        case J.fromJSON v of
          J.Success (moves :: P.PrevMoves a) -> do
            let move = Punter.play $ moves{ P.state = Just s' }
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
