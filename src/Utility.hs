module Utility(
    toFunction
) where

import qualified Data.Set as Set (Set)
import qualified Data.Map.Strict as Map (Map, lookup) 

toFunction :: (Ord k) => (Map.Map k v) -> (k -> v)
toFunction mp key = case Map.lookup key mp of (Just x) -> x
                                              Nothing  -> undefined

symmetric :: (Eq a) => (a, a) -> Bool
symmetric (x, y) = (x == y)

forceClear :: IO ()
forceClear = putStr $ take 100 $ repeat '\n'