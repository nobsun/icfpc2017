{-# OPTIONS_GHC -Wall #-}
import Data.Default.Class
import Data.List (intercalate)
import Data.Maybe
import Data.Monoid
import Options.Applicative
import qualified OfflinePlay
import qualified OnlinePlay
import qualified Punters as Punters

data Options = Options
  { optPunterName :: String
  , optHandshakeName :: Maybe String
  , optHost :: Maybe String
  , optPort :: Maybe String
  , optDumpMessages :: Bool
  , optDumpStates   :: Bool
  } deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> punterOption
  <*> nameOption
  <*> hostOption
  <*> portOption
  <*> dumpMessagesOption
  <*> dumpStatesOption
  where
    punterOption :: Parser String
    punterOption = strOption
      $  short 'p'
      <> long "punter"
      <> metavar "STRING"
      <> help ("punter name {" ++ intercalate "|" Punters.names ++ "}")

    nameOption :: Parser (Maybe String)
    nameOption = optional $ strOption
      $  long "name"
      <> metavar "STRING"
      <> help ("name for handshake")

    hostOption :: Parser (Maybe String)
    hostOption = optional $ strOption
      $  long "host"
      <> metavar "STRING"
      <> help ("hostname (default: " ++ OnlinePlay.optHostName def ++ ")")

    portOption :: Parser (Maybe String)
    portOption = optional $ strOption
      $  long "port"
      <> metavar "STRING"
      <> help ("port number")

    dumpMessagesOption :: Parser Bool
    dumpMessagesOption = switch
      $  long "dump-messages"
      <> help ("dump JSON messages")

    dumpStatesOption :: Parser Bool
    dumpStatesOption = switch
      $  long "dump-states"
      <> help ("dump punter states")

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser)
  $  fullDesc
  <> progDesc "Punter Runner"

main :: IO ()
main = do
  opt <- execParser parserInfo
  let m = Punters.withPunter (optPunterName opt) $ \punter -> do
         case optPort opt of
           Just s -> do
             let opt2 =
                   def
                   { OnlinePlay.optName        = fromMaybe (OnlinePlay.optName def) (optHandshakeName opt)
                   , OnlinePlay.optHostName    = fromMaybe (OnlinePlay.optHostName def) (optHost opt)
                   , OnlinePlay.optServiceName = s
                   , OnlinePlay.optDumpMessages = optDumpMessages opt
                   , OnlinePlay.optDumpStates   = optDumpStates opt
                   }
             OnlinePlay.runPunterOnline opt2 punter
           Nothing -> do
             let opt2 =
                   def
                   { OfflinePlay.optName = fromMaybe (OfflinePlay.optName def) (optHandshakeName opt)
                   , OfflinePlay.optDumpMessages = optDumpMessages opt
                   , OfflinePlay.optDumpStates   = optDumpStates opt
                   }
             OfflinePlay.runPunterOffline opt2 punter
  case m of
    Just act -> act
    Nothing -> error "unknown punter algorithm"
