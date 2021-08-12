module NFA(
    Sigma(Ch, Eps),
    NFA(NFA),
    states, delta, start, end,
    deltaF,
    follow,
    accepts,
    toDFA,
    fromDFA,
    intLabeling,
    stringLabeling,

    followEps,
    followChar,
    followCharClosed
) where

import Utility
import DFA ( DFA(DFA) )
import qualified DFA

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map



data Sigma = Ch Char | Eps deriving (Eq, Show)

instance Ord Sigma where
    compare Eps Eps = EQ
    compare _   Eps = GT
    compare Eps _   = LT
    compare (Ch a) (Ch b) = compare a b


data NFA state = NFA {
    states :: Set state,
    delta :: Map (Sigma, state) (Set state), -- (char, source) -> {targets}
    start :: state,
    end :: Set state
} deriving(Show)

deltaF :: (Ord state) => (NFA state) -> (Sigma -> state -> Set state)
deltaF nfa = curry f
    where
        delta0 = delta nfa
        f (ch, s) = case Map.lookup (ch, s) delta0 of
                        (Just x) -> x
                        Nothing  -> Set.empty


followChar :: (Ord state) => NFA state -> Sigma -> Set state -> Set state
followChar nfa ch s = Set.unions $ (Set.map) (dT ch) s
    where
        dT = deltaF nfa

isClosed :: (Ord state) => NFA state -> Set state -> Bool
isClosed nfa s = (followEps nfa s) == s

followEps :: (Ord state) => NFA state -> Set state -> Set state
followEps nfa s = bfs s s
    where
        dT = deltaF nfa
        bfs visited current = if Set.null current then visited else bfs visited' current'
            where
                visited' = Set.union visited current
                current' = Set.difference (followChar nfa Eps current) visited'

followCharClosed :: (Ord state) => NFA state -> Sigma -> Set state -> Set state
followCharClosed nfa ch = (fEps . fChar . fEps)
    where
        fEps = followEps nfa
        fChar = followChar nfa ch

follow :: (Ord state) => NFA state -> ([Sigma] -> Set state -> Set state)
follow nfa str s = foldr (followCharClosed nfa) s $ reverse str

accepts :: (Ord state) => NFA state -> String -> Bool
accepts nfa str0 = not $ Set.null $ finalAccepting
    where
        str1 = map Ch str0
        finalStates = follow nfa str1 (Set.singleton $ start nfa)
        finalAccepting = Set.intersection (end nfa) (finalStates)

fromDFA :: (Ord state) => DFA state -> NFA state
fromDFA dfa0 = NFA states1 delta1 start1 end1
    where
        states1 = DFA.states dfa0
        delta1  = Map.mapKeys (\(ch, s) -> (Ch ch, s)) 
                $ Map.map (Set.singleton) (DFA.delta dfa0)
        start1  = DFA.start dfa0
        end1    = DFA.end dfa0

toDFA :: (Ord state) => NFA state -> DFA (Set state)
toDFA nfa0 = DFA states1 delta1 start1 end1
    where
        states1 = Set.filter (isClosed0)
                $ Set.powerSet
                $ states nfa0

        start1  = followEps nfa0
                $ Set.singleton
                $ start nfa0

        end1    = Set.map (followEps nfa0)
                $ Set.filter (not. (Set.disjoint $ end nfa0))
                $ Set.powerSet
                $ end nfa0

        delta1  = Map.fromList
                $ map (\(ch, s) -> ((ch, s), followCharClosed nfa0 (Ch ch) s))
                $ Set.toList
                $ Set.cartesianProduct usedChars states1

        isClosed0 = isClosed nfa0

        usedChars = Set.fromList
                  $ map (\(Ch c) -> c)
                  $ filter (/=Eps) 
                  $ map fst
                  $ Map.keys
                  $ delta nfa0

intLabeling :: (Ord state) => NFA state -> NFA Int
intLabeling nfa0 = NFA states1 delta1 start1 end1
    where
        stateList = Set.toList $ states nfa0
        size0     = Set.size $ states nfa0
        rep       = toFunction
                  $ Map.fromAscList
                  $ zip stateList [0..]

        states1 = Set.fromAscList [0.. size0 - 1]
        start1  = rep $ start nfa0
        end1    = Set.mapMonotonic rep $ end nfa0
        delta1  = Map.mapKeysMonotonic (\(ch, s) -> (ch, rep s))
                $ Map.map (Set.mapMonotonic rep)
                $ delta nfa0

stringLabeling :: (Ord state) => NFA state -> NFA String
stringLabeling nfa0 = NFA states1 delta1 start1 end1
    where
        stateList = Set.toList $ states nfa0
        size0     = Set.size $ states nfa0
        rep       = toFunction
                  $ Map.fromAscList
                  $ zip stateList (map show [0..])

        states1 = Set.fromList $ map show [0.. size0 - 1]
        start1  = rep $ start nfa0
        end1    = Set.map rep $ end nfa0
        delta1  = Map.mapKeys (\(ch, s) -> (ch, rep s))
                $ Map.map (Set.map rep)
                $ delta nfa0
