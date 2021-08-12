module Regex(
    Regex(EmptyLang, ID, Ch, Star, Cat, Union),
    toNFA,
    fromNFA,
    toDFA,
    fromDFA,
    accepts,
    equivalent
) where

import Utility
import DFA (DFA(DFA))
import qualified DFA
import NFA (NFA(NFA), Sigma)
import qualified NFA
import AutomataOperations

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (sort, sortBy, group, groupBy)



data Regex = EmptyLang | ID | Ch Char | Star Regex | Cat Regex Regex | Union Regex Regex
    deriving (Eq)

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
fromNFA nfa0 = toRegex (toGNFA nfa0)

toGNFA :: (Ord state) => NFA state -> GNFA String
toGNFA nfa0 = GNFA states1 delta1 start1 end1
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
               $ (++) ((\x y -> ((x, y), EmptyLang)) <$> (Set.toList states1) <*> (Set.toList states1))
               $ map (\(dir, chs) -> (dir, foldr Union EmptyLang chs))
               $ map (\l -> (fst $ head l, map snd l))
               $ groupBy (\x y -> fst x == fst y)
               $ sortBy (\x y -> compare (fst x) (fst x))
               $ concatMap (\((ch, s0), ss1) -> map (\s ->((s0, s), sigmaToRegex ch)) (Set.toList ss1))
               $ Map.toList delta0


        sigmaToRegex :: NFA.Sigma -> Regex
        sigmaToRegex (NFA.Ch c) = Ch c
        sigmaToRegex (NFA.Eps) = ID


fromDFA :: (Ord state) => DFA state -> Regex
fromDFA dfa = fromNFA
            $ NFA.fromDFA
            $ DFA.toMinimal
            $ dfa

toDFA :: Regex -> DFA String
toDFA (Star x) = dfaStar (toDFA x)
toDFA (Union x y) = dfaUnion (toDFA x) (toDFA y)
toDFA (Cat x y) = dfaCat (toDFA x) (toDFA y)
toDFA EmptyLang = DFA.DFA (Set.singleton "") (Map.empty) "" (Set.empty)
toDFA ID = DFA.DFA (Set.singleton "") (Map.empty) "" (Set.singleton "")
toDFA (Ch c) = DFA.DFA (Set.fromList ["0", "1"])
                   (Map.fromList [((c, "0"), "1")])
                   "0"
                   (Set.singleton "1")

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

accepts :: Regex -> String -> Bool
accepts reg str = (NFA.accepts) (toNFA reg) str

equivalent :: Regex -> Regex -> Bool
equivalent reg1 reg2 = DFA.equivalent dfa1 dfa2
    where
        convert = DFA.toMinimal . toDFA
        dfa1 = convert reg1
        dfa2 = convert reg2
