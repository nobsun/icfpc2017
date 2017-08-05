{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OfflinePlay where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit)
import qualified Data.Aeson as J
import Data.Maybe
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import System.IO

import qualified Protocol as P
import Punter
import qualified Punter.Pass as PassPunter

test :: IO ()
test = runPunterOffline (Proxy :: Proxy PassPunter.Punter)

runPunterOffline :: Punter.IsPunter a => Proxy a -> IO ()
runPunterOffline punter = do
      hSetBuffering stdin (BlockBuffering Nothing)
      hSetBuffering stdout (BlockBuffering Nothing)
      runPunterOffline' "sampou-offline" punter

runPunterOffline' :: forall a. Punter.IsPunter a => T.Text -> Proxy a -> IO ()
runPunterOffline' name _ = do
  send (P.HandshakePunter{ P.me=name })
  (_::P.HandshakeServer) <- recv "handshake"
  setupInfo <- recv "setup"
  let (ready :: P.Ready a) = Punter.setup setupInfo
  let Just s = P.state (ready :: P.Ready a)
  send (ready{ P.state = Just s } :: P.Ready a)
  let loop :: a -> IO ()
      loop s' = do
        (v :: J.Value) <- recv "PrevMoves"
        case J.fromJSON v of
          J.Success (moves :: P.PrevMoves a) -> do
            let move = Punter.play $ moves{ P.state = Just s' }
            send (move :: P.MyMove a)
            loop $ fromJust $ P.state (move :: P.MyMove a)
          J.Error _ -> do
            case J.fromJSON v of
              J.Success (_ :: P.Scoring) -> return ()
              J.Error _ -> error ("unknown messsage: " ++ show v)
  loop s

send :: J.ToJSON a => a -> IO ()
send x = do
  let json = J.encode x
  B.hPutStrLn stderr ("-> " <> json)
  hFlush stderr
  B.hPutStr stdout $ B.pack (show (B.length json)) <> ":" <> json
  hFlush stdout

recv :: J.FromJSON a => String -> IO a
recv name = do
  len <- getLength []
  s <- B.hGet stdin len
  B.hPutStrLn stderr $ "<- " <> s
  case J.decode s of
    Nothing -> error ("failed to parse: " ++ name ++ ": " ++ show s)
    Just a -> return a
  where
    getLength cs = do
      c <- hGetChar stdin
      if isDigit c then getLength (c:cs) else return (read (reverse cs))
