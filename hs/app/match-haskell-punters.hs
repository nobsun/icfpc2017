{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LBS
import Data.Default.Class
import Data.List (intercalate)
import Data.Monoid
import Data.Proxy
import Options.Applicative
import System.Exit
import System.IO

import qualified Protocol as P
import qualified Punters as Punters
import qualified Simulator as Simulator

data Options = Options
  { optMap      :: String
  , optFutures  :: Bool
  , optSplurges :: Bool
  , optOptions  :: Bool
  , optTimeout  :: Maybe Int
  , optPunters  :: [String]
  } deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> mapOption
  <*> futuresOption
  <*> splurgesOption
  <*> optionsOption
  <*> timeoutOption
  <*> punters
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

    punters :: Parser [String]
    punters = some $ strArgument
      $  help ("punter names {" ++ intercalate "|" Punters.names ++ "}")
      <> metavar "STRING ..."

parserInfo :: ParserInfo Options
parserInfo = info (helper <*> optionsParser)
  $  fullDesc
  <> progDesc "Match Punters"

main :: IO ()
main = do
  opt <- execParser parserInfo
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
        { P.futures  = optFutures opt
        , P.splurges = optSplurges opt
        , P.options  = optOptions opt
        }

  punters <- forM (optPunters opt) $ \name -> do
    let m = Punters.withPunter name $ \(Proxy :: Proxy p) -> do
              (p :: Simulator.HaskellPunter p) <- Simulator.createHsPunter
              return $ Simulator.SomePunter p
    case m of
      Nothing -> error $ "unknown punter name: " ++ name
      Just act -> act

  let opt2 = def{ Simulator.optPlayTimeout = fromIntegral <$> optTimeout opt }
  Simulator.simulate opt2 map' settings punters
