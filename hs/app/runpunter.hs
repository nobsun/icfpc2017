{-# OPTIONS_GHC -Wall #-}
import Data.List (intercalate)
import Data.Monoid
import Options.Applicative
import OfflinePlay (runPunterOffline)
import OnlinePlay (runPunterOnline)
import qualified Punters as Punters

data Options = Options
  { optPunterName :: String
  , _optHandShakeName :: Maybe String
  , optPort :: Maybe String
  } deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> punterOption
  <*> nameOption
  <*> portOption
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

    portOption :: Parser (Maybe String)
    portOption = optional $ strOption
      $  long "port"
      <> metavar "STRING"
      <> help ("port number")

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser)
  $  fullDesc
  <> progDesc "Punter Runner"

main :: IO ()
main = do
  opt <- execParser parserInfo
  let m = Punters.withPunter (optPunterName opt) $ \punter -> do
         case optPort opt of
           Just s -> runPunterOnline punter s
           Nothing -> runPunterOffline punter
  case m of
    Just act -> act
    Nothing -> error "unknown punter algorithm"
