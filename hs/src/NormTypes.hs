{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module NormTypes ( NRiver
                 , toNRiver
                 , deNRiver
                 , NClaim
                 , toNClaim
                 ) where

import Protocol

import GHC.Generics
import Data.Aeson
import Data.Aeson.Types

newtype NRiver = NRiver (SiteId, SiteId)

toNRiver' :: SiteId -> SiteId -> NRiver
toNRiver' s t = if s < t then NRiver (s, t) else NRiver (t, s)

toNRiver :: River -> NRiver
toNRiver (River s t) = toNRiver' s t

deNRiver :: NRiver -> (SiteId, SiteId)
deNRiver (NRiver p) = p

data NClaim = NClaim
  { claimer :: PunterId
  , nriver  :: NRiver
  }

toNClaim :: (PunterId, SiteId, SiteId) -> NClaim
toNClaim (pid, src, tar) = NClaim pid $ toNRiver' src tar
