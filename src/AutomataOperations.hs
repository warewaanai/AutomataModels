module AutomataOperations(
    markStates,
    nfaStar,
    dfaStar,
    nfaCat,
    dfaCat,
    nfaUnion,
    dfaUnion
) where

import Utility
import DFA (DFA(DFA))
import qualified DFA
import NFA (NFA(NFA), Sigma)
import qualified NFA

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sort, sortBy, group, groupBy)

markStates :: Char -> NFA String -> NFA String
markStates ch nfa0 = NFA states1 delta1 start1 end1
    where
        states1 = Set.mapMonotonic (ch:) (NFA.states nfa0)
        start1  = (ch : NFA.start nfa0)
        end1    = Set.mapMonotonic (ch:) (NFA.end nfa0)
        delta1  = Map.mapKeysMonotonic (\(tch, s) -> (tch, ch:s))
                $ Map.map (\s -> Set.mapMonotonic (ch:) s)
                $ NFA.delta nfa0

nfaStar :: NFA String -> NFA String
nfaStar nfa0 = NFA states1 delta1 start1 end1
    where
        (NFA states0 delta0 start0 end0) = markStates '0' nfa0
        states1 = Set.insert "*" states0
        start1  = "*"
        end1    = Set.insert "*" end0
        delta1  = Map.fromList
                $ (++) [((NFA.Eps, "*"), Set.singleton start0)] 
                $ (++) (map (\e -> ((NFA.Eps, e), Set.singleton start0)) (Set.toList end0))
                $ Map.toList delta0

nfaUnion :: NFA String -> NFA String -> NFA String
nfaUnion nfa0 nfa1 = NFA states2 delta2 start2 end2
    where
        (NFA states0 delta0 start0 end0) = markStates '0' nfa0
        (NFA states1 delta1 start1 end1) = markStates '1' nfa1
        states2 = Set.unions [Set.singleton "*", states0, states1]
        start2  = start0
        end2    = end1
        delta2  = Map.unions [newTransitions, delta0, delta1]

        newTransitions = Map.fromList [((NFA.Eps, "*"), Set.singleton start0), ((NFA.Eps, "*"), Set.singleton start1)]

nfaCat :: NFA String -> NFA String -> NFA String
nfaCat nfa0 nfa1 = NFA states2 delta2 start2 end2
    where
        (NFA states0 delta0 start0 end0) = markStates '0' nfa0
        (NFA states1 delta1 start1 end1) = markStates '1' nfa1
        states2 = Set.unions [states0, states1]
        start2  = start0
        end2    = end1
        delta2  = Map.unions [newTransitions, delta0, delta1]

        newTransitions = Map.fromList 
                        $ map (\s -> ((NFA.Eps, s), Set.singleton $ start1))
                        $ Set.toList end0


dfaProduct :: (Ord state) => DFA state -> DFA state -> (Bool -> Bool -> Bool) -> DFA (state, state)
dfaProduct dfa0 dfa1 pred = dfa2
    where
        (DFA states0 delta0 start0 end0) = dfa0
        (DFA states1 delta1 start1 end1) = dfa1
        states2 = Set.cartesianProduct states0 states1
        start2  = (start0, start1)
        end2    = Set.filter pred
                $ Set.cartesianProduct end0 end1
        delta2  = Map.fromList
                $ concatMap (\(((ch0, src0), trg0), ((ch1, src1), trg1)) -> if ch0 == ch1 then [((ch0, (src0, src1)), (trg0, trg1))] else [])
                $ (\x y -> (x, y)) <$> Map.toList (DFA.delta dfa0) <*> Map.toList (DFA.delta dfa1)

dfaStar :: DFA String -> DFA String -- there must be something more efficient than this
dfaStar dfa0 = DFA.toMinimal
             $ DFA.stringLabeling
             $ NFA.toDFA
             $ nfaStar
             $ NFA.fromDFA dfa0

dfaUnion :: DFA String -> DFA String -> DFA String
dfaUnion dfa0 dfa1 = DFA.toMinimal
                   $ DFA.stringLabeling
                   $ NFA.toDFA
                   $ nfaUnion (NFA.fromDFA dfa0) (NFA.fromDFA dfa1)


dfaCat :: DFA String -> DFA String -> DFA String
dfaCat dfa0 dfa1 = DFA.toMinimal
                 $ DFA.stringLabeling
                 $ NFA.toDFA
                 $ nfaCat (NFA.fromDFA dfa0) (NFA.fromDFA dfa1)
