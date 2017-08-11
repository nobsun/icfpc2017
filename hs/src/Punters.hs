{-# LANGUAGE Rank2Types #-}
module Punters where

import Data.Proxy
import Punter
import qualified Punter.Pass as PassPunter
import qualified Punter.Any as AnyPunter
import qualified Punter.Greedy as GreedyPunter
import qualified Punter.MaxDegree as MaxDegree
import qualified Punter.Alternate as Alternate
import qualified Punter.STF as STF
import qualified Punter.Longest as Longest
import qualified Punter.Connector as Connector
import qualified Punter.MixedStrategy as Mixed

names :: [String]
names =
  [ "pass"
  , "any"
  , "greedy"
  , "max-degree"
  , "greedy2"
  , "stf"
  , "longest"
  , "connector"
  , "mixed"
  ]

withPunter :: String -> (forall a. Punter.IsPunter a => Proxy a -> b) -> Maybe b
withPunter name k =
  case name of
    "pass" -> Just $ k (Proxy :: Proxy PassPunter.Punter)
    "any" -> Just $ k (Proxy :: Proxy AnyPunter.Punter)
    "greedy" -> Just $ k (Proxy :: Proxy GreedyPunter.Punter)
    "max-degree" -> Just $ k (Proxy :: Proxy MaxDegree.Punter)
    "greedy2" -> Just $ k (Proxy :: Proxy (Alternate.Alternate MaxDegree.Punter GreedyPunter.Punter))
    "stf" -> Just $ k (Proxy :: Proxy STF.Punter)
    "longest" -> Just $ k (Proxy :: Proxy Longest.Punter)
    "connector" -> Just $ k (Proxy :: Proxy Connector.Punter)
    "mixed" -> Just $ k (Proxy :: Proxy Mixed.Punter)
    _ -> Nothing
