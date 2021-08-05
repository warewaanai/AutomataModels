module Regex(
    Regex(EmptyLang, ID, Ch, Star, Cat, Union),
    toNFA,
    fromNFA,
    toDFA,
    fromDFA,
    accepts,
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



data Regex = EmptyLang | ID | Ch Char | Star Regex | Cat Regex Regex | Union Regex Regex

markStates :: Char -> NFA String -> NFA String
markStates ch nfa0 = NFA states1 delta1 start1 end1
    where
        states1 = Set.mapMonotonic (ch:) (NFA.states nfa0)
        start1 = (ch : NFA.start nfa0)
        end1 = Set.mapMonotonic (ch:) (NFA.end nfa0)
        delta1 = Map.mapKeysMonotonic (\(tch, s) -> (tch, ch:s))
               $ Map.map (\s -> Set.mapMonotonic (ch:) s)
               $ NFA.delta nfa0

nfaStar :: NFA String -> NFA String
nfaStar nfa0 = NFA states1 delta1 start1 end1
    where
        (NFA states0 delta0 start0 end0) = markStates '0' nfa0
        states1 = Set.insert "" $ states0
        start1 = ""
        end1 = Set.insert "" $ end1
        delta1 = Map.insert (NFA.Eps, "") (Set.singleton (NFA.start nfa0)) delta0

nfaUnion :: NFA String -> NFA String -> NFA String
nfaUnion nfa0 nfa1 = undefined

nfaCat :: NFA String -> NFA String -> NFA String
nfaCat nfa0 nfa1 = NFA states2 delta2 start2 end2
    where
        (NFA states0 delta0 start0 end0) = markStates '0' nfa0
        (NFA states1 delta1 start1 end1) = markStates '1' nfa1
        states2 = Set.unions [Set.singleton "*", states0, states1]
        start2  = start0
        end2    = end1
        delta2  = Map.unions [newTransitions, delta0, delta1]

        newTransitions = Map.fromList 
                        $ map (\s -> ((NFA.Eps, s), Set.singleton $ start1))
                        $ Set.toList end0


toNFA :: Regex -> NFA String
toNFA (Star x) = nfaStar (toNFA x)
toNFA (Union x y) = nfaUnion (toNFA x) (toNFA y)
toNFA (Cat x y) = nfaCat (toNFA x) (toNFA y)
toNFA EmptyLang = NFA (Set.singleton "") (Map.empty) "" (Set.empty)
toNFA ID = NFA (Set.singleton "") (Map.empty) "" (Set.singleton "")
toNFA (Ch c) = NFA (Set.fromList ["0", "1"]) 
                   (Map.fromList [((NFA.Ch c, "0"), Set.singleton "1")]) 
                   "0"
                   (Set.singleton "1")

data GNFA state = GNFA {
    states :: Set state, -- all states *except* start and end
    delta :: state -> state -> Regex, -- (source) -> (target) -> (edge label)
    start :: state,
    end :: state
}

toRegex :: (Ord state) => GNFA state -> Regex
toRegex gnfa | Set.null (states gnfa)   = dT (start gnfa) (end gnfa)
             | otherwise                = toRegex reduced
    where
        (killedState, states1) = Set.deleteFindMin $ states gnfa
        dT = delta gnfa

        reduced      = GNFA states1 delta1 (start gnfa) (end gnfa)
        starKilled   = Star $ dT killedState killedState
        delta1 s1 s2 = Union (dT s1 s2) ((dT s1 killedState) `Cat` starKilled `Cat` (dT killedState s2))


fromNFA :: (Ord state) => NFA state -> Regex
fromNFA nfa0 = toRegex $ GNFA states1 delta1 start1 end1
    where
        (NFA states0 delta0 start0 end0) = markStates '0' $ NFA.stringLabeling nfa0
        states1 = states0
        start1 = "start"
        end1 = "end"
        delta1 src trg | src == start1 = if trg == start0 then ID else EmptyLang
                       | trg == end1   = if Set.member trg end0 then ID else EmptyLang
                       | otherwise     = delta' (src, trg)

        delta' = toFunction
               $ Map.fromList
               $ map (\(dir, chs) -> (dir, foldr Union EmptyLang chs))
               $ map (\l -> (fst $ head l, map snd l))
               $ groupBy (\x y -> fst x == fst y)
               $ sortBy (\x y -> compare (fst x) (fst x))
               $ concatMap (\((ch, s0), s1) -> map (\s ->((s0, s), sigmaToRegex ch)) (Set.toList s1))
               $ Map.toList delta0


        sigmaToRegex :: NFA.Sigma -> Regex
        sigmaToRegex (NFA.Ch c) = Ch c
        sigmaToRegex (NFA.Eps) = ID


fromDFA :: (Ord state) => DFA state -> Regex
fromDFA dfa = fromNFA $ NFA.fromDFA dfa

toDFA :: Regex -> DFA (Set String) -- To Replace for Efficiency (mitigate the exponential number of states with )
toDFA reg = NFA.toDFA $ toNFA reg

accepts :: Regex -> String -> Bool
accepts reg str = (NFA.accepts) (toNFA reg) str
