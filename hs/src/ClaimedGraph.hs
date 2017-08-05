module ClaimedGraph
       ( growByPunter, growClaimed
       ) where

import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Graph.Inductive.Graph (DynGraph, insNode, insEdge, hasEdge)

import Protocol (PunterId, )
import NormTypes (NRiver, deNRiver, NClaim, )


growByPunter :: (Show (gr () ()), DynGraph gr)
             => NRiver -> gr () () -> gr () ()
growByPunter nr g0
    | g0 `hasEdge` e  =  error $ "growClaimed: Bug? To insert an already inserted edge: " ++ show e ++ ", " ++ show g0
    | otherwise       =  insEdge (x, y, ()) . insNode (y, ()) $ insNode (x, ()) g0
  where
    e@(x, y) = deNRiver nr

growClaimed :: (Show (gr () ()), DynGraph gr)
            => [NClaim] -> [(PunterId, gr () ())] -> [(PunterId, gr () ())]
growClaimed ccs0 ggs0 = recurse (sortBy (comparing fst) ccs0) (sortBy (comparing fst) ggs0)
  where
    recurse ccs@((pix, r):cs) ggs@(g@(piy, rg):gs)
      | pix  <  piy  =  recurse cs ggs
      | pix  >  piy  =  g : recurse ccs gs
      | otherwise    =  (piy, growByPunter r rg) : recurse cs gs
    recurse (_:_)             []    =  []
    recurse []                ggs   =  ggs
