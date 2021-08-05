module TM(
    Sigma,
    Gamma(InputChar, InternalChar, Eps),
    Dir,
    DTM(DTM),
    TAPE,
    step,
    steps,
    accepts,
    result,
) where

import Utility

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


type Sigma = Char
data Gamma g = InputChar Sigma | InternalChar g | Eps deriving(Eq)
data Dir = L | R deriving(Eq)
type HeadPos = Int

data DTM state g = DTM {
    states :: Set state,
    startState :: state,
    delta :: state -> Gamma g -> (state, Gamma g, Dir),
    accepting :: state,
    rejecting :: state
}

type Tape g = Map HeadPos (Gamma g)

readTape :: (Ord g) => Tape g -> HeadPos -> Gamma g
readTape tape pos = case lookup pos tape of Just x  -> x
                                            Nothing -> Eps

writeTape :: (Ord g) => Tape g -> HeadPos -> Gamma g -> Tape g
writeTape tape pos ch = insert pos ch tape

toInputTape :: (Ord g) => String -> Tape g
toInputTape str = Map.fromList $ zip [0..] (map S str)

toIterator :: (Ord state, Ord g) => DTM state g -> (state, Tape g, HeadPos) -> Either String (state, Tape g, HeadPos)
toIterator tm (s0, tape0, pos0) = res
    where
        dT = toFunction $ delta tm

        (s1, ch1, dir1) = dT s0 (tapeRead tape0 pos0)
        pos1           = if dir1 == L then pos1 - 1 else pos1 + 1
        tape1          = writeTape tape0 pos0 ch1
        res            = if      s0 == acceptng tm || s0 == rejecting tm then Left "Iteration Attempt After Execution Halted" 
                         else if pos1 < 0                                then Left "Tape Head Moved to Negative Position"
                                                                         else Right (s1, t1, pos1)


toIterations :: (Ord state, Ord g) => (Ord state, Ord g) => DTM state g -> (state, Tape g, HeadPos) -> Either String [(state, Tape g, HeadPos)]
toIterations tm (s, tape, pos) = takeWhile isRight
                               $ iterate (\arg -> arg >>= iterator) (s, tape, pos)
    where
        iterator = toIterator tm


accepts :: (Ord state, Ord g) => DTM state g -> Tape g -> Bool
accepts tm tape = (last $ toIterations tm) == accepting tm

rejects :: (Ord state, Ord g) => DTM state g -> Tape g -> Bool
rejects tm tape = (last $ toIterations tm) == rejecting tm

result :: (Ord state, Ord g) => DTM state g -> Tape g -> Tape g
result tm tape = let (_, tapeFinal, _) = (last $ toIterations tm) in tapeFinal
