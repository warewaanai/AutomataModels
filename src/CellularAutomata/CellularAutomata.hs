module Cellular(
    Cellular,
    Grid,
    advance
) where

import Utility
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map


data Cellular state cell = Cellular {
    zeroState :: state,
    neighbours :: cell -> Set cell,
    transition :: cell -> state -> state
}

type Grid cell state = Map cell state

advance :: (Ord cell, Eq state) => (Cellular cell state) -> Grid cell state -> Grid cell state
advance automaton grid0 = grid1
    where
        allStates = Map.unions -- all active cells and their neighbours
                  $ activeCells0 : map (\cell -> neighbours automaton cell)
                  $ Map.toList activeCells0

        grid1 = Map.filterWithKey (\cell state -> not (state == zeroState automaton && all (==zeroState automaton) (Set.toList $ neighbours cell))) -- remove from grid cells that are dead as well and their neighbours are too
              $ Map.mapWithKey transition allStates
