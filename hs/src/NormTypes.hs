{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module NormTypes ( NRiver
                 , toNRiver'
                 , toNRiver
                 , deNRiver
                 , riverSet

                 , NClaim
                 , toNClaim
                 ) where

import Control.DeepSeq
import Data.Set (Set)
import qualified  Data.Set as Set
import Data.Aeson
import GHC.Generics

import Protocol (River (River), PunterId, SiteId)


newtype NRiver =
  NRiver (SiteId, SiteId)
  deriving (Eq, Ord, Show, Generic, NFData)

toNRiver' :: SiteId -> SiteId -> NRiver
toNRiver' s t = if s < t then NRiver (s, t) else NRiver (t, s)

toNRiver :: River -> NRiver
toNRiver (River s t) = toNRiver' s t

deNRiver :: NRiver -> (SiteId, SiteId)
deNRiver (NRiver p) = p

instance ToJSON NRiver where
  toJSON = toJSON . uncurry River . deNRiver

instance FromJSON NRiver where
  parseJSON = (toNRiver <$>) . parseJSON

riverSet :: [River] -> Set NRiver
riverSet = Set.fromList . map toNRiver

type NClaim = (PunterId, NRiver)

toNClaim :: (PunterId, SiteId, SiteId) -> NClaim
toNClaim (pid, src, tar) = (pid, toNRiver' src tar)
