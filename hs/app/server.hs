{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Exception
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LBS
import Data.Default.Class
import Data.Monoid
import Network.Socket
import Options.Applicative
import System.Exit
import System.IO

import qualified Protocol as P
import qualified Simulator as Simulator

data Options = Options
  { optMap        :: String
  , optFutures    :: Bool
  , optSplurges   :: Bool
  , optOptions    :: Bool
  , optTimeout    :: Maybe Int
  -- , optHost :: String
  , optPort       :: String
  , optNumPunters :: Int
  } deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> mapOption
  <*> futuresOption
  <*> splurgesOption
  <*> optionsOption
  <*> timeoutOption
--  <*> hostOption
  <*> portOption
  <*> numPuntersOption
  where
    mapOption :: Parser String
    mapOption = strOption
      $  long "map"
      <> metavar "FILENAME"
      <> help ("map filename (e.g. lambda.json)")

    futuresOption :: Parser Bool
    futuresOption = switch
      $  long "futures"
      <> help "enable futures"

    splurgesOption :: Parser Bool
    splurgesOption = switch
      $  long "splurges"
      <> help "enable splurges"

    optionsOption :: Parser Bool
    optionsOption = switch
      $  long "options"
      <> help "enable options"

    timeoutOption :: Parser (Maybe Int)
    timeoutOption = optional $ option auto
      $  long "timeout"
      <> metavar "N"
      <> help "time limit in each move (seconds)"

    numPuntersOption :: Parser Int
    numPuntersOption = option auto
      $  short 'n'
      <> metavar "N"
      <> help "number of punters"

{-
    hostOption :: Parser String
    hostOption = strOption
      $  long "host"
      <> metavar "STRING"
      <> help ("hostname")
-}

    portOption :: Parser String
    portOption = strOption
      $  long "port"
      <> metavar "STRING"
      <> help ("port number")

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser)
  $  fullDesc
  <> progDesc "Punter server"

main :: IO ()
main = do
  opt <- execParser parserInfo

  let hints = defaultHints { addrFamily = AF_INET6, addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  addrs@(addr:_) <- getAddrInfo (Just hints) Nothing (Just (optPort opt))
  print addr
  case addrAddress addr of
    SockAddrInet6 _ _ a _ -> print a
    _ -> return ()

  bracket (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 5
  
    map' <- do
      bs <- LBS.readFile (optMap opt)
      case J.eitherDecode bs of
        Right map' -> return map'
        Left err -> do
          hPutStrLn stderr $ "failed to parse a map file " ++ optMap opt
          hPutStrLn stderr err
          exitFailure
    let settings =
          P.Settings
          { P.futures  = Just $ optFutures opt
          , P.splurges = Just $ optSplurges opt
          , P.options  = Just $ optOptions opt
          }
    let opt2 = def{ Simulator.optPlayTimeout = fromIntegral <$> optTimeout opt }
  
    _ <- forever $ do
      punters <- replicateM (optNumPunters opt) $ do
        (sock2, _) <- accept sock
        h2 <- socketToHandle sock2 ReadWriteMode
        Simulator.createOnlinePunter h2
      Simulator.simulate opt2 map' settings punters

    return ()
