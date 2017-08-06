
module Punter where

-- import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Protocol as P

class (ToJSON a, FromJSON a) => IsPunter a where
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

  logger :: a -> IO a
  logger = return

play :: IsPunter a => P.PrevMoves a -> IO (P.MyMove a)
play (P.PrevMoves moves (Just a)) = return . chooseMove =<< logger (applyMoves moves a)
play (P.PrevMoves _moves Nothing) = return $ error "Punter.play: no state is available"

{- --- JSON の decode 結果の可否で区別できるので必要にならなかった
data OfflineStage
  = Setup
  | GamePlay Int
  --- | Scoring
  deriving (Eq, Ord, Show, Generic)

instance ToJSON OfflineStage
instance FromJSON OfflineStage

class IsPunter a => IsOfflinePunter a where
  offlineStage :: a -> OfflineStage
 -}
