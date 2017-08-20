{-# LANGUAGE GADTs #-}
module Punter where

import System.IO
import Control.DeepSeq
import Data.Aeson (ToJSON, FromJSON)
import qualified Protocol as P

class (ToJSON a, FromJSON a, NFData a) => IsPunter a where
  setup :: P.Setup -> P.Ready a

  -- 自分を含む各Punterの前ターンの手の情報を受け取って情報を更新
  applyMoves :: P.Moves -> a -> a

  -- chooseMoveを実装するのに通常はこちらを実装する
  chooseMoveSimple :: a -> P.Move

  -- 基本的にはchooseMoveSimpleを実装することを想定していて、
  -- 状態を更新することは想定していないが、
  -- コストの大きい探索結果を保存したい場合などにはこちらを定義する
  chooseMove :: a -> P.MyMove a
  chooseMove a = P.MyMove (chooseMoveSimple a) (Just a)

  dumpState :: (String -> IO ()) -> a -> IO ()
  dumpState _ _ = return ()

play :: IsPunter a => (String -> IO ()) -> P.PrevMoves a -> IO (P.MyMove a)
play logger (P.PrevMoves moves (Just a)) = do
  let s = applyMoves moves a
  dumpState logger s
  return $ chooseMove s
  
play _ (P.PrevMoves _moves Nothing) = return $ error "Punter.play: no state is available"

data SomePunter where
  SomePunter :: IsPunter p => p -> SomePunter
