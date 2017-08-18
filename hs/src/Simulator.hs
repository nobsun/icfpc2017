{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
TODO:
* futures の計算
* ゾンビ化
-}
module Simulator
  ( simulate
  , Options (..)

  , IsPunterAdapter (..)
  , SomePunter (..)

  , HaskellPunter (..)
  , createHsPunter

  , OfflinePunter (..)
  , createOfflinePunter

  , OnlinePunter (..)
  , createOnlinePunter
  )
  where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Default.Class
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IORef
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import System.Clock
import System.IO
import System.IO.Error
import System.Process
import System.Timeout
import Text.Printf

import qualified Protocol as P
import qualified Punter as Punter
import qualified CommonState as CommonState
import qualified DistanceTable as DistanceTable

type S = (CommonState.MovePool, IntMap P.Move)

data Options
  = Options
  { optSetupTimeout :: Maybe Double
  , optPlayTimeout :: Maybe Double
  } deriving (Show, Eq, Ord)

instance Default Options where
  def =
    Options
    { optSetupTimeout = Nothing
    , optPlayTimeout = Nothing
    }

simulate :: IsPunterAdapter p => Options -> P.Map -> P.Settings -> [p] -> IO ()
simulate opt map' settings punters' = do
  let punters = IntMap.fromList (zip [0..] punters')
      numPunters = IntMap.size punters
      distTable = DistanceTable.mkDistanceTable map'

  putStrLn "setup"
  (futures :: IntMap DistanceTable.Futures) <- liftM IntMap.fromList $ forM (IntMap.toList punters) $ \(pid, punter) -> do
    let setupArg =
          P.Setup
          { P.punter   = pid
          , P.punters  = numPunters
          , P.map      = map'
          , P.settings = Just settings
          }
    futures <- setup punter setupArg (optSetupTimeout opt)
    return (pid, IntMap.fromList [(mine, site) | P.Future mine site <- futures])

  let x0 :: S
      x0 =
        ( CommonState.empty numPunters map' settings
        , IntMap.mapWithKey (\pid _ -> P.MvPass pid) punters
        )

      step :: S -> Int -> IO S
      step (state, prev) pid = do
        ret <- tryIOError $ play (punters IntMap.! pid) (optPlayTimeout opt)
        case ret of
          Right (Just (m, tm)) -> do
            forM_ (IntMap.elems punters) $ \p -> addMove p m
            LBS8.putStrLn $
              "  punter " <> LBS8.pack (show pid) <> ": " <>
              "move=" <> J.encode m <> ", " <>
              "time=" <> LBS8.pack (printf "%0.3f" tm) <> "sec"
            return $
              ( CommonState.applyMove m state
              , IntMap.insert pid m prev
              )
          Right Nothing -> do
            let m = P.MvPass pid
            forM_ (IntMap.elems punters) $ \p -> addMove p m
            LBS8.putStrLn $
              "  punter " <> LBS8.pack (show pid) <> ": " <>
              "move=" <> J.encode m <> ", " <>
              "timeout"
            return $
              ( CommonState.applyMove m state
              , IntMap.insert pid m prev
              )
          Left err -> do
            let m = P.MvPass pid
            forM_ (IntMap.elems punters) $ \p -> addMove p m
            LBS8.putStrLn $
              "  punter " <> LBS8.pack (show pid) <> ": " <>
              "move=" <> J.encode m <> ", " <>
              "error=" <> LBS8.pack (show err)
            return $
              ( CommonState.applyMove m state
              , IntMap.insert pid m prev
              )

      totalSteps :: Int
      totalSteps = length (P.rivers map')

      dump :: Int -> S -> IO ()
      dump n (state, prev) = do
        let prevMoves :: P.PrevMoves ()
            prevMoves =
              P.PrevMoves
              { P.move = P.Moves (IntMap.elems prev)
              , P.state = Nothing
              }
        putStrLn $ "turn " ++ show n
        LBS8.putStrLn $ "<- " <> J.encode prevMoves
        forM_ [0..numPunters-1] $ \pid -> do
          let m = CommonState.scoreOf state distTable futures pid
          putStrLn $ "  punter " ++ show pid ++ ": score=" ++ show m

      loop :: Int -> S -> IO S
      loop n x
        | n + numPunters < totalSteps = do
            dump n x
            x' <- foldM step x [0..numPunters-1]
            loop (n + numPunters) x'
        | otherwise = do
            dump n x
            (state, prev) <- foldM step x $ take (totalSteps - n) [0..numPunters-1]
            let stopMsg :: P.Stop
                stopMsg =
                  P.Stop
                  { P.moves  = [if pid < totalSteps - n then prev IntMap.! pid else P.MvPass pid | pid <- [0..numPunters-1]]
                  , P.scores = [P.Score pid (fromIntegral $ CommonState.scoreOf state distTable futures pid) | pid <- [0..numPunters-1]]
                  }
            putStrLn "stop"
            LBS8.putStrLn $ "<- " <> J.encode stopMsg
            return
              ( state
              , prev `IntMap.union` IntMap.fromList [(pid, P.MvPass pid) | pid <- [0..numPunters-1], pid >= totalSteps - n]
              )

  (state, prev) <- loop 0 x0

  let scores = IntMap.fromList [(pid, CommonState.scoreOf state distTable futures pid) | pid <- [0..numPunters-1]]

      stopMsg :: P.Stop
      stopMsg =
        P.Stop
        { P.moves  = (IntMap.elems prev)
        , P.scores = [P.Score pid (fromIntegral s) | (pid,s) <- IntMap.toList scores]
        }
  putStrLn "stop"
  LBS8.putStrLn $ "<- " <> J.encode stopMsg

  forM_ [0..numPunters-1] $ \pid -> do
    stop (punters IntMap.! pid) scores

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

softTimeout :: Maybe Int -> IO a -> IO (Maybe a)
softTimeout Nothing m = Just <$> m
softTimeout (Just lim) m = do
  tm1 <- getTime Monotonic
  a <- m
  tm2 <- getTime Monotonic
  if toNanoSecs (tm2 `diffTimeSpec` tm1) >= fromIntegral lim then
    return Nothing
  else
    return (Just a)

recv :: (J.FromJSON a) => Handle -> IO a
recv h = do
  n <- getLength []
  s <- LBS.hGet h n
  case J.decode s of
    Nothing -> error ("failed to parse " ++ show s)
    Just a -> return a
  where
    getLength cs = do
      c <- hGetChar h
      if c /= ':' then getLength (c:cs) else readIO (reverse cs)

send :: J.ToJSON a => Handle -> a -> IO ()
send h a = do
  let json = J.encode a
  LBS.hPutStr h $ LBS8.pack (show (LBS.length json))
  LBS.hPutStr h ":"
  LBS.hPutStr h json
  hFlush h

-- ------------------------------------------------------------------------

class IsPunterAdapter p where
  setup
    :: p
    -> P.Setup
    -> Maybe Double -- ^ timeout
    -> IO (P.Futures)

  -- | ^ play 時に送るために事前に登録しておく
  addMove
    :: p
    -> P.Move
    -> IO ()

  play
    :: p
    -> Maybe Double -- ^ timeout
    -> IO (Maybe (P.Move, Double))

  stop
    :: p
    -> IntMap DistanceTable.Score
    -> IO ()

-- ------------------------------------------------------------------------

data SomePunter where
  SomePunter :: IsPunterAdapter p => p -> SomePunter

instance IsPunterAdapter SomePunter where
  setup (SomePunter p) = setup p
  addMove (SomePunter p) = addMove p
  play (SomePunter p) = play p
  stop (SomePunter p) = stop p

-- ------------------------------------------------------------------------

data HaskellPunter p
  = HaskellPunter
  { hpPunterId  :: IORef P.PunterId
  , hpState     :: IORef p
  , hpMoveQueue :: IORef [P.Move]
  }

createHsPunter :: Punter.IsPunter p => IO (HaskellPunter p)
createHsPunter = do
  pidRef <-  newIORef undefined
  stateRef <- newIORef undefined
  moveQueue <- newIORef []
  return $
    HaskellPunter
    { hpPunterId  = pidRef
    , hpState     = stateRef
    , hpMoveQueue = moveQueue
    } 

instance Punter.IsPunter p => IsPunterAdapter (HaskellPunter p) where
  -- TODO: timeoutを無視してる
  setup p setupArg _timelim = do
    let P.ReadyOn{ P.ready = pid, P.state = Just state, P.futures = futures }  = Punter.setup setupArg
    writeIORef (hpPunterId p) pid
    writeIORef (hpState p) state
    return $ fromMaybe [] futures

  addMove p move = modifyIORef (hpMoveQueue p) (move :)

  play p timelim = do
    state <- readIORef (hpState p)
    moves <- readIORef (hpMoveQueue p)
    let prevMoves =
          P.PrevMoves
          { P.move  = P.Moves (reverse moves)
          , P.state = Just state
          }
    evaluate $ rnf prevMoves
    ret <- timeout' (fmap (round . (10^(6::Int) *)) timelim) $ measureSec $ do
      myMove <- Punter.play prevMoves
      evaluate $ rnf myMove
      return myMove
    case ret of
      Nothing -> return Nothing
      Just (P.MyMove{ P.state = Nothing }, _) -> error "no state is available"
      Just (P.MyMove{ P.move = m, P.state = Just state' }, tm) -> do
        writeIORef (hpState p) state'
        writeIORef (hpMoveQueue p) []
        return $ Just (m, tm)

  stop _p _scores = return ()

-- ------------------------------------------------------------------------

data OfflinePunter
  = OfflinePunter
  { offCommand   :: String
  , offPunterId  :: IORef P.PunterId
  , offState     :: IORef J.Value
  , offMoveQueue :: IORef [P.Move]
  }

createOfflinePunter :: String -> IO OfflinePunter
createOfflinePunter cmd = do
  pidRef <-  newIORef undefined
  stateRef <- newIORef undefined
  moveQueue <- newIORef []
  return $
    OfflinePunter
    { offCommand   = cmd
    , offPunterId  = pidRef
    , offState     = stateRef
    , offMoveQueue = moveQueue
    } 

withPunterProcess :: String -> (Handle -> Handle -> ProcessHandle -> IO a) -> IO a
withPunterProcess cmd k = do
  let cp = (shell cmd){ std_in = CreatePipe, std_out = CreatePipe, std_err = Inherit } -- std_err は NoStream にしたいけど、それだとエラーになる
  withCreateProcess cp $ \(Just h1) (Just h2) _ ph -> do
    P.HandshakePunter{ P.me = name } <- recv h2
    send h1 $ P.HandshakeServer{ P.you = name }
    k h1 h2 ph

instance IsPunterAdapter OfflinePunter where
  -- TODO: timeoutを無視してる
  setup p setupArg _timelim = do
    withPunterProcess (offCommand p) $ \h1 h2 _ph -> do
      send h1 $ setupArg
      P.ReadyOn{ P.ready = pid, P.state = Just state, P.futures = futures } <- recv h2
      writeIORef (offPunterId p) pid
      writeIORef (offState p) state
      return $ fromMaybe [] futures

  addMove p move = modifyIORef (offMoveQueue p) (move :)

  play p timelim = do
    state <- readIORef (offState p)
    moves <- readIORef (offMoveQueue p)
    let prevMoves =
          P.PrevMoves
          { P.move  = P.Moves (reverse moves)
          , P.state = Just state
          }
    evaluate $ rnf prevMoves
    ret <- timeout' (fmap (round . (10^(6::Int) *)) timelim) $ measureSec $ do
      withPunterProcess (offCommand p) $ \h1 h2 _ph -> do
        send h1 prevMoves
        myMove <- recv h2
        evaluate $ rnf myMove
        return myMove
    case ret of
      Nothing -> return Nothing
      Just (P.MyMove{ P.state = Nothing }, _) -> error "no state is available"
      Just (P.MyMove{ P.move = m, P.state = Just state' }, tm) -> do
        writeIORef (offState p) state'
        writeIORef (offMoveQueue p) []
        return $ Just (m, tm)

  stop _p _scores = return ()


-- ------------------------------------------------------------------------

data OnlinePunter
  = OnlinePunter
  { onHandle    :: Handle
  , onName      :: T.Text
  , onPunterId  :: IORef P.PunterId
  , onMoveQueue :: IORef [P.Move]
  }

createOnlinePunter :: Handle -> IO OnlinePunter
createOnlinePunter h = do
  P.HandshakePunter{ P.me = name } <- recv h
  send h $ P.HandshakeServer{ P.you = name }
  pidRef <-  newIORef undefined
  moveQueue <- newIORef []
  return $
    OnlinePunter
    { onHandle    = h
    , onName      = name
    , onPunterId  = pidRef
    , onMoveQueue = moveQueue
    } 

instance IsPunterAdapter OnlinePunter where
  -- TODO: timeoutを無視してる
  setup p setupArg _timelim = do
    send (onHandle p) $ setupArg
    (P.ReadyOn{ P.ready = pid, P.futures = futures } :: P.Ready J.Value) <- recv (onHandle p)
    writeIORef (onPunterId p) pid
    return $ fromMaybe [] futures

  addMove p move = modifyIORef (onMoveQueue p) (move :)

  play p timelim = do
    moves <- readIORef (onMoveQueue p)
    let prevMoves :: P.PrevMoves ()
        prevMoves =
          P.PrevMoves
          { P.move  = P.Moves (reverse moves)
          , P.state = Nothing
          }
    evaluate $ rnf prevMoves
    ret <- softTimeout (fmap (round . (10^(6::Int) *)) timelim) $ measureSec $ do
      send (onHandle p) prevMoves
      (myMove :: P.MyMove J.Value) <- recv (onHandle p)
      evaluate $ rnf myMove
      return myMove
    case ret of
      Nothing -> do
        send (onHandle p) (P.Timeout (fromJust timelim))
        return Nothing
      Just (P.MyMove{ P.move = m }, tm) -> do
        writeIORef (onMoveQueue p) []
        return $ Just (m, tm)

  stop p scores = do
    moves <- readIORef (onMoveQueue p)
    let stopMsg :: P.Stop
        stopMsg =
          P.Stop
          { P.moves  = reverse moves
          , P.scores = [P.Score pid (fromIntegral s) | (pid, s) <- IntMap.toList scores]
          }
    send (onHandle p) P.Scoring{ P.stop = stopMsg }
    hClose (onHandle p)

-- ------------------------------------------------------------------------
