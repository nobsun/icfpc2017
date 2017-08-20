{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OfflinePlay where

import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isDigit)
import Data.Aeson (Result (Success, Error))
import qualified Data.Aeson as J
import Data.Default.Class
import Data.Monoid
import Data.Proxy
import qualified Data.Text as T
import System.IO
import System.Timeout (timeout)
import System.Exit (exitSuccess)

import qualified Protocol as P
import Punter
import qualified Punter.Pass as PassPunter

test :: IO ()
test = runPunterOffline def (Proxy :: Proxy PassPunter.Punter)

data Options
  = Options
  { optName        :: String
  }
  deriving (Show, Eq)

instance Default Options where
  def =
    Options
    { optName        = "sampou-offline"
    }

runPunterOffline :: Punter.IsPunter a => Options -> Proxy a -> IO ()
runPunterOffline opt punter = do
      hSetBuffering stdin (BlockBuffering Nothing)
      hSetBuffering stdout (BlockBuffering Nothing)
      runPunterOffline' (T.pack (optName opt)) punter

runPunterOffline' :: forall a. Punter.IsPunter a => T.Text -> Proxy a -> IO ()
runPunterOffline' name _ =
    loop
  where
    loop = do
      send (P.HandshakePunter{ P.me=name })
      (_::P.HandshakeServer) <- recv "handshake"
      jsonv <- recv "multiplex"
      case J.fromJSON jsonv :: Result (ServerMsg a) of
        Error _err -> fail $ "unknown messsage: " ++ show jsonv
        Success (SMPrevMoves moves) -> (send =<< Punter.play moves)               *> loop
        Success (SMSetup setupInfo) -> send (Punter.setup setupInfo :: P.Ready a) *> loop
        Success (SMScoring _)       -> pure ()

data ServerMsg a
  = SMSetup P.Setup
  | SMPrevMoves (P.PrevMoves a)
  | SMScoring P.Scoring
  deriving (Show)

instance J.FromJSON a => J.FromJSON (ServerMsg a) where
  parseJSON v
    =   (SMPrevMoves <$> J.parseJSON v) -- check gameplay first
    <|> (SMSetup <$> J.parseJSON v)
    <|> (SMScoring <$> J.parseJSON v)

instance J.ToJSON a => J.ToJSON (ServerMsg a) where
  toJSON (SMSetup x) = J.toJSON x
  toJSON (SMPrevMoves x) = J.toJSON x
  toJSON (SMScoring x) = J.toJSON x

send :: J.ToJSON a => a -> IO ()
send x = do
  let json = J.encode x
  L8.hPutStrLn stderr ("-> " <> json)
  hFlush stderr
  L8.hPutStr stdout $ L8.pack (show (L8.length json)) <> ":" <> json
  hFlush stdout

processTimeoutSecond :: Int
processTimeoutSecond = 180

recv :: J.FromJSON a => String -> IO a
recv name = do
  s  <- (maybe (hPutStrLn stderr "OfflinePlay: Timeout of waiting input. exiting." *> exitSuccess) return =<<) .
        timeout (processTimeoutSecond * 1000 * 1000) $ do
    len <- getLength []
    L8.hGet stdin len
  L8.hPutStrLn stderr $ "<- " <> s
  maybe
    (fail $ "failed to parse: " ++ name ++ ": " ++ show s)
    return
    $ J.decode s
  where
    getLength cs = do
      c <- hGetChar stdin
      if isDigit c then getLength (c:cs) else readIO (reverse cs)
