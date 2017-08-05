{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module UnionFind
  ( Class (..)
  , classSize
  , classToList

  , Table
  , emptyTable
  , getRepr
  , getClass
  , getClasses
  , unify
  ) where

import qualified Data.Aeson as J
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import GHC.Generics

data Class
  = ClassSingleton !Int
  | ClassUnion !Int Class Class
  deriving (Generic, Show)

instance J.ToJSON Class
instance J.FromJSON Class

classSize :: Class -> Int
classSize (ClassSingleton _) = 1
classSize (ClassUnion size _ _) = size

classUnion :: Class -> Class -> Class
classUnion as bs = ClassUnion (classSize as + classSize bs) as bs

classToList :: Class -> [Int]
classToList cls = f cls []
  where
    f (ClassSingleton a)   = (a :)
    f (ClassUnion _ as bs) = f as . f bs

data Table
  = Table
  { reprs   :: IntMap Int
  , classes :: IntMap Class
  }
  deriving (Generic, Show)

instance J.ToJSON Table
instance J.FromJSON Table

emptyTable :: Table
emptyTable =
  Table
  { reprs = IntMap.empty
  , classes = IntMap.empty
  }

getRepr :: Table -> Int -> Int
getRepr tbl = loop
  where
    loop !x =
      case IntMap.lookup x (reprs tbl) of
        Nothing -> x
        Just y -> loop y

getClass :: Table -> Int -> Class
getClass tbl a = getClass' tbl (getRepr tbl a)

getClass' :: Table -> Int -> Class
getClass' tbl a = IntMap.findWithDefault (ClassSingleton a) a (classes tbl)

getClasses :: Table -> IntMap Class
getClasses = classes

unify :: Table -> Int -> Int -> Table
unify tbl a b
  | a'==b' = tbl
  | classSize as > classSize bs =
      Table
      { reprs = IntMap.union (IntMap.fromList [(b'', a') | b'' <- classToList bs]) (reprs tbl) -- IntMap.union is left-biases
      , classes = IntMap.insert a' (classUnion as bs) $ IntMap.delete b' $ classes tbl
      }
  | otherwise =
      Table
      { reprs = IntMap.union (IntMap.fromList [(a'', b') | a'' <- classToList as]) (reprs tbl) -- IntMap.union is left-biases
      , classes = IntMap.insert b' (classUnion as bs) $ IntMap.delete a' $ classes tbl
      }
  where
    a' = getRepr tbl a
    b' = getRepr tbl b
    as = getClass' tbl a'
    bs = getClass' tbl b'
