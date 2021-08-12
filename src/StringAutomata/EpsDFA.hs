module EpsDFA (
    Sigma(Ch, Eps),
    EpsDFA(EpsDFA),
    states, delta, start, end,
    deltaF,
    accepts,
    followGreedy
) where

import Utility

import Data.List (group, groupBy, sort, sortBy)
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

data EpsDFA state = EpsDFA {
    states :: Set state,
    delta :: Map (Sigma, state) state,
    start :: state,
    end :: Set state
} deriving(Show)

deltaF :: (Ord state) => EpsDFA state -> Sigma -> state -> state
deltaF dfa = curry $ toFunction $ delta dfa

accepts :: (Ord state) => EpsDFA state -> String -> Bool
accepts auto str0 = undefined

edgeList :: (Ord state) => EpsDFA state -> [(state, Sigma, state)] -- [ ( from, ch, to ) ]
edgeList dfa = sort $ map (\((ch, from), to) -> (from, ch, to)) (Map.toList $ delta dfa)

transitions :: (Ord state) => EpsDFA state -> [(state, [(Sigma, state)])] -- [(from, [(ch, to)])]
transitions dfa = map (\lst -> (fst $ head lst, map snd lst))
                $ groupBy (\x y -> fst x == fst y)
                $ map (\(x, y, z) -> (x, (y, z)))
                $ edgeList dfa

followGreedy :: KMP -> State -> Char -> State
followGreedy automaton state ch' = res
    where
        ch = Ch ch'
        transition = Map.lookup (ch, state) (delta automaton)
        failState  = case Map.lookup (Eps, state) (delta automaton) of (Just m) -> m
        res | transition == Nothing && state == 0 = 0
            | transition == Nothing && state /= 0 = followGreedy automaton failState ch'
            | otherwise = case transition of (Just x) -> x
