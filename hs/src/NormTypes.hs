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

data NRiver = NRiver
  { node1 :: SiteId
  , node2 :: SiteId
  }

toNRiver :: River -> NRiver
toNRiver (River s t) = if s < t then NRiver s t else NRiver t s

deNRiver :: NRiver -> (SiteId,SiteId)
deNRiver (NRiver s t) = (s, t)

data NClaim = NClaim
  { claimer :: PunterId
  , nriver  :: NRiver
  }

toNClaim :: Claim -> NClaim
toNClaim (Claim pid src tar)
  | src < tar  = NClaim pid (NRiver src tar)
  | otherwise  = NClaim pid (NRiver tar src)
