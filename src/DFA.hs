module DFA (
    Sigma,
    DFA(DFA),
    states, delta, start, end,
    deltaF,
    accepts,
    reduce,
    isomorphic,
    equivalent,
    intLabeling,
    stringLabeling,
    normalLabeling
) where

import Utility


import Data.Either (isLeft)
import Data.List (group, groupBy, sort, sortBy)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map



type Sigma = Char

data DFA state = DFA {
    states :: Set state,
    delta :: Map (Sigma, state) state,
    start :: state,
    end :: Set state
}


deltaF :: (Ord state) => DFA state -> Sigma -> state -> state
deltaF dfa = curry $ toFunction $ delta dfa

accepts :: (Ord state) => DFA state -> String -> Bool
accepts auto str = Set.member (foldr dT (start auto) str) (end auto)
    where
        dT = deltaF auto

edgeList :: (Ord state) => DFA state -> [(state, Sigma, state)] -- [ ( from, ch, to ) ] 
edgeList dfa = sort $ map (\((ch, from), to) -> (from, ch, to)) (Map.toList $ delta dfa)

transitions :: (Ord state) => DFA state -> [(state, [(Sigma, state)])] -- [(from, [(ch, to)])]
transitions dfa = map (\lst -> (fst $ head lst, map snd lst))
                $ groupBy (\x y -> fst x == fst y)
                $ map (\(x, y, z) -> (x, (y, z)))
                $ edgeList dfa


reduce :: (Ord state) => DFA state -> Either (DFA state) (DFA state) -- applies a single reduction iteration
reduce dfa0 = if states dfa0 == states dfa1 then Left dfa1 else Right dfa1
    where
--      equivalenceClasses :: [ [state] ] -- [ (["nodes with identical transitions"]) ]
        equivalenceClasses = map (map fst)
                           $ groupBy (\(_, x) (_, y) -> x == y)
                           $ sortBy (\x y -> compare (snd x) (snd y))
                           $ map (\(st, trs) -> (st, sort trs) )
                           $ transitions dfa0

--      representativeMap :: Map state state -- maps nodes to the representative of their equivalence classes (which is arbitrarily chosen)
        representativeMap = Map.fromList 
                          $ concat
                          $ map (\eqclass -> map (\x -> (x, head eqclass)) eqclass) equivalenceClasses

        rep = toFunction representativeMap

--      dfa1 :: DFA state
        dfa1 = DFA states1 delta1 start1 end1
            where
                states1 = Set.map rep (states dfa0)
                start1 = rep (start dfa0)
                end1 = Set.map rep (end dfa0)
                delta1  = Map.fromList
                        $ map (\(from, ch, to) -> ((ch, from), to))
                        $ map head
                        $ group
                        $ sort
                        $ map (\(from, ch, to) -> (rep from, ch, rep to))
                        $ edgeList dfa0


toMinimal :: (Ord state) => DFA state -> DFA state
toMinimal dfa = case until isLeft (\arg -> arg >>= reduce) (Right dfa) of (Left  x) -> x

isomorphic :: (Ord state) => DFA state -> DFA state -> Bool
isomorphic dfa1 dfa2 = equal (normalLabeling dfa1) (normalLabeling dfa2)

equivalent :: (Ord state) => DFA state -> DFA state -> Bool
equivalent dfa1 dfa2 = isomorphic (toMinimal $ normalLabeling dfa1) (toMinimal $ normalLabeling dfa2)

equal :: (Ord state) => DFA state -> DFA state -> Bool
equal dfa1 dfa2 = (start dfa1 == start dfa2) &&
                  (end dfa1 == end dfa2) &&
                  (states dfa1 == states dfa2) &&
                  (Map.toList (delta dfa1) == Map.toList (delta dfa2))


dfs :: (Ord node) => (node -> [node]) -> node -> [node]
dfs nghF now = reverse
             $ snd
             $ dfs' (Set.singleton now) now
    where
    --  dfs' returns a pair (visited, dfs traversal)
        dfs' visited now = foldr advance (visited, [now]) (nghF now)
            where
                advance nxt (v, t) = if Set.member nxt v
                                     then (v, t)
                                     else let (v', t') = dfs' (Set.insert nxt visited) nxt in (v', t' ++ t)

intLabeling :: (Ord state) => DFA state -> DFA Int
intLabeling dfa0 = DFA states1 delta1 start1 end1
    where
        stateList = Set.toList $ states dfa0
        size0     = Set.size $ states dfa0
        rep       = toFunction
                  $ Map.fromAscList
                  $ zip stateList [0..]

        states1 = Set.fromAscList [0.. size0 - 1]
        start1  = rep $ start dfa0
        end1    = Set.mapMonotonic rep $ end dfa0
        delta1  = Map.mapKeysMonotonic (\(ch, s) -> (ch, rep s))
                $ Map.map rep
                $ delta dfa0

stringLabeling :: (Ord state) => DFA state -> DFA String
stringLabeling dfa0 = DFA states1 delta1 start1 end1
    where
        stateList = Set.toList $ states dfa0
        size0     = Set.size $ states dfa0
        rep       = toFunction
                  $ Map.fromAscList
                  $ zip stateList
                  $ map show [0..]

        states1 = Set.fromList $ map show [0.. size0 - 1]
        start1  = rep $ start dfa0
        end1    = Set.mapMonotonic rep $ end dfa0
        delta1  = Map.mapKeys (\(ch, s) -> (ch, rep s))
                $ Map.map rep
                $ delta dfa0

normalLabeling :: (Ord state) => DFA state -> DFA Int
normalLabeling dfa0 = DFA states1 delta1 start1 end1
    where
        statesList = reverse $ dfs delta' (start dfa0)

        delta' = toFunction -- delta' returns a list whose order is independent of the original labeling
                $ Map.fromList
                $ map (\(n, tr_n) -> (n, map snd $ sort tr_n))
                $ transitions dfa0

        rep = toFunction $ Map.fromList $ zip statesList [0..]

        start1  = rep $ start dfa0
        end1    = Set.map rep $ end dfa0
        states1 = Set.fromAscList [0 .. (Set.size $ states dfa0) - 1]
        delta1  = Map.fromList
                $ map (\((x, y), z) -> ((x, rep y), rep z))
                $ Map.toList
                $ delta dfa0