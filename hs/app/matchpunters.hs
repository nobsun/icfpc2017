{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (intercalate)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Monoid
import Data.Proxy
import Options.Applicative
import System.Exit
import System.IO

import qualified Protocol as P
import qualified Punter as Punter
import qualified Punters as Punters
import qualified CommonState as CommonState
import qualified ScoreTable as ScoreTable

data Options = Options
  { optMap      :: String
  , optFutures  :: Bool
  , optSplurges :: Bool
  , optOptions  :: Bool
  , optPunters  :: [String]
  } deriving (Eq, Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> mapOption
  <*> futuresOption
  <*> splurgesOption
  <*> optionsOption
  <*> some punter
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

    punter :: Parser String
    punter = strArgument
      $  help ("punter name {" ++ intercalate "|" Punters.names ++ "}")
      <> metavar "STRING"

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
        { P.futures  = Just $ optFutures opt
        , P.splurges = Just $ optSplurges opt
        , P.options  = Just $ optOptions opt
        }
      numPunters = length (optPunters opt)
      scoreTable = ScoreTable.mkScoreTable map'

  xs <- forM (zip [0..] (optPunters opt)) $ \(pid, name) -> do
    let setupArg =
          P.Setup
          { P.punter   = pid
          , P.punters  = numPunters
          , P.map      = map'
          , P.settings = Just settings
          }
    let m = Punters.withPunter name $ \(Proxy :: Proxy p) -> 
              case Punter.setup setupArg :: P.Ready p of
                P.ReadyOn{ P.state = Nothing } -> error "should not happen"
                P.ReadyOn{ P.state = Just p, P.futures = futures } ->
                  return (pid, Punter.SomePunter p, futures)
    case m of
      Nothing -> error $ "unknown punter name: " ++ name
      Just act -> act

  let x0 :: (IntMap (Punter.SomePunter, P.Move), CommonState.MovePool)
      x0 =
        ( IntMap.fromList [(pid, (p, P.MvPass pid)) | (pid, p, _) <- xs]
        , CommonState.empty numPunters map' settings
        )

      step
        :: (IntMap (Punter.SomePunter, P.Move), CommonState.MovePool)
        -> Int
        -> IO (IntMap (Punter.SomePunter, P.Move), CommonState.MovePool)
      step (punterInfo, state) pid = do
        case punterInfo IntMap.! pid of
          (Punter.SomePunter p, _) -> do
            let prevMoves =
                  P.PrevMoves
                  { P.move  = P.Moves [m | (_, m) <- IntMap.elems punterInfo]
                  , P.state = Just p
                  }
            P.MyMove{ P.move = m, P.state = Just p' } <- Punter.play prevMoves
            return $
              ( IntMap.insert pid (Punter.SomePunter p', m) punterInfo
              , CommonState.applyMove m state
              )

      totalSteps :: Int
      totalSteps = length (P.rivers map')

      dump :: Int -> (IntMap (Punter.SomePunter, P.Move), CommonState.MovePool) -> IO ()
      dump n (punterInfo, state) = do
        let prevMoves :: P.PrevMoves ()
            prevMoves =
              P.PrevMoves
              { P.move = P.Moves [m | (_, m) <- IntMap.elems punterInfo]
              , P.state = Nothing
              }
        putStrLn $ "turn " ++ show n
        LBS8.putStrLn $ "<- " <> J.encode prevMoves
        forM_ [0..numPunters-1] $ \pid -> do
          let m = CommonState.scoreOf state scoreTable pid
          putStrLn $ "  punter " ++ show pid ++ ": score=" ++ show m

      loop
        :: Int
        -> (IntMap (Punter.SomePunter, P.Move), CommonState.MovePool)
        -> IO (IntMap (Punter.SomePunter, P.Move), CommonState.MovePool)
      loop n x
        | n + numPunters < totalSteps = do
            dump n x
            x' <- foldM step x [0..numPunters-1]
            loop (n + numPunters) x'
        | otherwise = do
            dump n x
            x' <- foldM step x $ take (totalSteps - n) [0..numPunters-1]
            let stop :: P.Stop
                stop =
                  P.Stop
                  { P.moves  = [if pid < totalSteps - n then snd (fst x' IntMap.! pid) else P.MvPass pid | pid <- [0..numPunters-1]]
                  , P.scores = [P.Score pid (fromIntegral $ CommonState.scoreOf (snd x') scoreTable pid) | pid <- [0..numPunters-1]]
                  }
            putStrLn "stop"
            LBS8.putStrLn $ "<- " <> J.encode stop
            return x'

  _ <- loop 0 x0
  return ()
  
