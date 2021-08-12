module Aho (
    Aho,
    epsAho,
    toAho,
    matches
) where

import EpsDFA (EpsDFA(EpsDFA), Sigma(Ch, Eps), states, delta, end, start)
import qualified EpsDFA
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

type State = Int
data Aho = Aho {
    automaton EpsDFA State,
    patternMap Map String State
} deriving (Show)

epsAho = Aho (EpsDFA (Set.singleton 0) (Map.fromList [((Eps, 0), 0)]) 0 Set.empty) (Map.empty)

ahoSize aho = Set.size $ states $ automaton aho

toAho :: [String] -> Aho
toAho = undefined

matches :: String -> Aho -> Map String [Int]
matches = undefined

