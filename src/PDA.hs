module PDA(
    Sigma(Ch, Eps),
    Gamma(Ch, Eps)
    PDA(PDA),
    states, delta, start, end,
    deltaF,
    follow,
    accepts
) where

import Utility

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

type Gamma = Sigma
type Stack = [Gamma]

data PDA state = PDA {
    states :: Set state,
    delta :: Map (state, Sigma, Gamma) ([Gamma], state)
    start :: state,
    end :: Set state
}

deltaF :: (Ord state) => PDA state -> ((state, Sigma, Gamma) -> Maybe ([Gamma], state))
deltaF pda (s, chS, chG) = Map.lookup (s, chS, chG) (delta pda)

isEpsState :: (Ord state) => PDA state -> state -> Bool
isEpsState pda s = case Map.lookupGT (state, Eps, Eps) of Just ((state, Eps, _)) -> True
                                                          _                      -> False
    