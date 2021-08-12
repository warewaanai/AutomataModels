module Main where

import qualified Data.Set as Set
import Regex
import qualified DFA
import qualified NFA
import Parsing

test1 :: IO () -- regex to dfa twice
test1 = do
    let reg1 = Regex.Cat (Regex.Ch '0') (Regex.Ch '1')
    let dfa1 = Regex.toDFA reg1
    let reg2 = Regex.fromDFA dfa1
    let dfa2 = Regex.toDFA reg2

    print $ reg2
--    putStrLn $ if DFA.equivalent dfa1 dfa3 then "GREAT SUCCESS!" else "NAUGHTY NAUGHTY!"

main :: IO ()
main = do
    test1
