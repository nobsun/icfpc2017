{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Control.DeepSeq
import Control.Exception
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
import System.Clock
import System.Exit
import System.IO
import System.Timeout
import Text.Printf

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

type S = (IntMap (Punter.SomePunter, P.Move), CommonState.MovePool, IntMap [P.Move])

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

  putStrLn "setup"
  xs <- forM (zip [0..] (optPunters opt)) $ \(pid, name) -> do
    let setupArg =
          P.Setup
          { P.punter   = pid
          , P.punters  = numPunters
          , P.map      = map'
          , P.settings = Just settings
          }
    let m = Punters.withPunter name $ \(Proxy :: Proxy p) -> do
              (ready, tm) <- measureSec $ do
                let ready = Punter.setup setupArg :: P.Ready p
                evaluate $ rnf $ J.encode ready -- P.Ready自体はNFDataにしていないので文字列化したものを評価
                return ready
              LBS8.putStrLn $
                "  punter " <> LBS8.pack (show pid) <> ": " <>
                "time=" <> LBS8.pack (printf "%0.3f" tm) <> "sec"
              case ready of
                P.ReadyOn{ P.state = Nothing } -> error "should not happen"
                P.ReadyOn{ P.state = Just p, P.futures = futures } ->
                  return (pid, Punter.SomePunter p, futures)
    case m of
      Nothing -> error $ "unknown punter name: " ++ name
      Just act -> act

  let x0 :: S
      x0 =
        ( IntMap.fromList [(pid, (p, P.MvPass pid)) | (pid, p, _) <- xs]
        , CommonState.empty numPunters map' settings
        , IntMap.fromList [(pid,[]) | pid <- [0..numPunters-1]]
        )

      step :: S -> Int -> IO S
      step (punterInfo, state, notifyQueue) pid = do
        case punterInfo IntMap.! pid of
          (Punter.SomePunter (p :: p), _) -> do
            let prevMoves =
                  P.PrevMoves
                  { P.move  = P.Moves (reverse (notifyQueue IntMap.! pid))
                  , P.state = Just p
                  }
            -- P.PrevMoves自体はNFDataにしていないのでとりあえず評価を強制しない
            -- evaluate $ rnf prevMoves
            ret <- timeout' ((10^(6::Int) *) <$> optTimeout opt) $ measureSec $ do
              myMove <- Punter.play prevMoves
              let moveStr = J.encode (P.move (myMove :: P.MyMove p))
              evaluate $ rnf moveStr -- P.Move自体はNFDataにしていないので文字列化したものを評価
              return (myMove, moveStr)
            case ret of
              Just ((P.MyMove{ P.state = Nothing }, _), _) -> error "no state is available"
              Just ((P.MyMove{ P.move = m, P.state = Just p' }, moveStr), tm) -> do
                LBS8.putStrLn $
                  "  punter " <> LBS8.pack (show pid) <> ": " <>
                  "move=" <> moveStr <> ", " <>
                  "time=" <> LBS8.pack (printf "%0.3f" tm) <> "sec"
                return $
                  ( IntMap.insert pid (Punter.SomePunter p', m) punterInfo
                  , CommonState.applyMove m state
                  , IntMap.map (m :) $ IntMap.insert pid [] $ notifyQueue
                  )
              Nothing -> do
                let m = P.MvPass pid
                    moveStr = J.encode m
                LBS8.putStrLn $
                  "  punter " <> LBS8.pack (show pid) <> ": " <>
                  "move=" <> moveStr <> ", " <>
                  "timeout"
                return $
                  ( IntMap.insert pid (Punter.SomePunter p, m) punterInfo
                  , CommonState.applyMove m state
                  , IntMap.map (m :) $ notifyQueue
                  )

      totalSteps :: Int
      totalSteps = length (P.rivers map')

      dump :: Int -> S -> IO ()
      dump n (punterInfo, state, _notifyQueue) = do
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

      loop :: Int -> S -> IO S
      loop n x
        | n + numPunters < totalSteps = do
            dump n x
            x' <- foldM step x [0..numPunters-1]
            loop (n + numPunters) x'
        | otherwise = do
            dump n x
            x'@(punterInfo, state, _notifyQueue) <- foldM step x $ take (totalSteps - n) [0..numPunters-1]
            let stop :: P.Stop
                stop =
                  P.Stop
                  { P.moves  = [if pid < totalSteps - n then snd (punterInfo IntMap.! pid) else P.MvPass pid | pid <- [0..numPunters-1]]
                  , P.scores = [P.Score pid (fromIntegral $ CommonState.scoreOf state scoreTable pid) | pid <- [0..numPunters-1]]
                  }
            putStrLn "stop"
            LBS8.putStrLn $ "<- " <> J.encode stop
            return x'

  _ <- loop 0 x0
  return ()
  
measureSec :: IO a -> IO (a, Double)
measureSec io = do
  tm1 <- getTime Monotonic
  a <- io
  tm2 <- getTime Monotonic
  return (a, fromIntegral (toNanoSecs (tm2 `diffTimeSpec` tm1)) / 10^(9::Int))

timeout' :: Maybe Int -> IO a -> IO (Maybe a)
timeout' Nothing m    = Just <$> m
timeout' (Just lim) m = timeout lim m
