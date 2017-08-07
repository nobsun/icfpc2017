{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OfflinePlay where

import Control.Applicative
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char (isDigit)
import Data.Aeson (Result (Success, Error))
import qualified Data.Aeson as J
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
test = runPunterOffline (Proxy :: Proxy PassPunter.Punter)

runPunterOffline :: Punter.IsPunter a => Proxy a -> IO ()
runPunterOffline punter = do
      hSetBuffering stdin (BlockBuffering Nothing)
      hSetBuffering stdout (BlockBuffering Nothing)
      runPunterOffline' "sampou-offline" punter

runPunterOffline' :: forall a. Punter.IsPunter a => T.Text -> Proxy a -> IO ()
runPunterOffline' name _ =
    loop
  where
    loop = do
      send (P.HandshakePunter{ P.me=name })
      (_::P.HandshakeServer) <- recv "handshake"
      jsonv <- recv "multiplex"

      (maybe (fail $ "unknown messsage: " ++ show jsonv) pure =<<) . runMaybeT $
        (play' =<< prevmoves' jsonv)
        <|>     --- check gameplay first
        (sendSetup =<< setup' jsonv)
        <|>
        (const (pure ()) =<< scoring' jsonv)
        
    -- define
    recv' js = result (const empty) pure (J.fromJSON js)
    prevmoves' :: J.Value -> MaybeT IO (P.PrevMoves a)
    scoring' ::  J.Value -> MaybeT IO P.Scoring
    setup' :: J.Value -> MaybeT IO P.Setup
    (prevmoves', scoring', setup') = (recv', recv', recv')
    sendSetup :: P.Setup -> MaybeT IO ()
    sendSetup si  = lift $ send (Punter.setup si :: P.Ready a) *> loop
    play' :: P.PrevMoves a -> MaybeT IO ()
    play' mvs =  lift (send =<< Punter.play mvs *> loop)
    
result :: (String -> b) -> (a -> b) -> Result a -> b
result f g r = case r of
  Error   s  ->  f s
  Success x  ->  g x

send :: J.ToJSON a => a -> IO ()
send x = do
  let json = J.encode x
  L8.hPutStrLn stderr ("-> " <> json)
  hFlush stderr
  L8.hPutStr stdout $ L8.pack (show (L8.length json)) <> ":" <> json
  hFlush stdout

processTimeoutSecond :: Int
processTimeoutSecond = 600

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
