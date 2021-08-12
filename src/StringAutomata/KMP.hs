module KMP (
    KMP,
    epsKMP,
    toKMP,
    matches
) where

import EpsDFA (EpsDFA(EpsDFA), Sigma(Ch, Eps), states, delta, end, start, followGreedy)

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map



type KMP = EpsDFA Int
type State = Int

epsKMP = EpsDFA (Set.singleton 0) (Map.fromList [((Eps, 0), 0)]) 0 Set.empty

kmpSize kmp = Set.size $ states kmp


append :: Char -> (KMP, State) -> (KMP, State)
append ch (automaton0, failState0) = (automaton1, failState1)
    where
        (EpsDFA states0 delta0 start0 end0) = automaton0
        newHead = kmpSize automaton0
        oldHead = newHead - 1

        failState1 = if oldHead == 0 then 0 else followGreedy automaton0 failState0 ch

        automaton1 = EpsDFA states1 delta1 start1 end1
        states1    = Set.insert newHead states0
        start1     = start0
        end1       = Set.singleton newHead
        delta1     = Map.union delta0
                   $ Map.fromList [((Eps, newHead), failState1), ((Ch ch, oldHead), newHead)]



toKMP :: String -> KMP
toKMP = fst . foldr append (epsKMP, 0) . reverse

matches :: String -> KMP -> [Int]
matches str automaton = res
    where
        patternSize = kmpSize automaton - 1
        states = scanl (followGreedy automaton) 0 str

        res = map (\x -> x - patternSize)
            $ map snd
            $ filter (\(s, pos) -> s == patternSize) 
            $ zip states [0..]
