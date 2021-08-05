module Cellular(
    Cellular,
    coway
) where

import Utility

import Set(Set)
import Qualified Set


data Cellular state cell = Cellular {
    zeroState :: state,
    neighbours :: cell -> Set cell,
    transition :: cell -> state -> state
}


advance :: (Ord cell, Eq state) => (Cellular cell state) -> Map cell state -> Map cell state
advance automaton activeCells = filter (/= (zeroState automaton))
                              $ map (uncurry $ transition automaton)
                              $ Map.toList activeCells

data ConwayState = Dead | Alive
type ConwayCell = (Int, Int)

conway :: Cellular ConwayCell ConwayState
conway = Cellular undefined undefined undefined
