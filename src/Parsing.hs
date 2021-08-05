module Parsing(
    show,
    toConcrete
) where

import Utility
import Regex

import Data.Char (isAlpha)
import Data.List (intersperse)

{-------- Show Regex --------}

bracket :: String -> String
bracket x = ("(" ++ x ++ ")")

-- data Regex = EmptyLang | ID | Ch Char | Star Regex | Cat Regex Regex | Union Regex Regex
data CRegex = CUnion [CRegex] | CWord String | CCat [CRegex] | CStar CRegex



rmEmpty :: [CRegex] -> [CRegex]
rmEmpty xs = filter (not.isEmptyLang) xs
    where
        isEmptyLang :: CRegex -> Bool
        isEmptyLang (CUnion []) = True
        isEmptyLang _           = False

toConcrete :: Regex -> CRegex
toConcrete (Union a b) = reduceUnion (toConcrete a) (toConcrete b)
toConcrete (Cat a b) = reduceCat (toConcrete a) (toConcrete b)
toConcrete (Star x) = CStar $ toConcrete x
toConcrete (Ch x) = CWord [x]
toConcrete ID = CWord ""
toConcrete EmptyLang = CUnion []

reduceCat :: CRegex -> CRegex -> CRegex
reduceCat (CCat x) (CCat y)   = CCat (x ++ y)
reduceCat x (CCat y)          = CCat (x:y)
reduceCat (CCat x) y          = CCat (y:x)
reduceCat (CWord x) (CWord y) = CWord (x ++ y)
reduceCat x y                 = CCat [x, y]

reduceUnion :: CRegex -> CRegex -> CRegex
reduceUnion (CUnion x) (CUnion y) = CUnion $ rmEmpty (x ++ y)
reduceUnion (CUnion x) y          = CUnion $ rmEmpty (y:x)
reduceUnion x (CUnion y)          = CUnion $ rmEmpty (x:y)
reduceUnion x y                   = CUnion $ rmEmpty [x, y]

instance Show CRegex where
    show (CWord x)  = x

    show (CUnion x) = case (length x) of
                        0 -> ""
                        1 -> show $ head x
                        _ -> bracket $ concat $ intersperse "+" $ map show x

    show (CCat x) = case (length x) of
                        0 -> ""
                        1 -> show $ head x
                        _ -> bracket $ concat $ map show x

    show (CStar x) = case x of
                        (CUnion []) -> ""
                        (CWord [c]) -> [c, '*']
                        (CStar _ )  -> show x
                        _           -> bracket (show x) ++ "*"

instance Show Regex where
    show reg = show $ toConcrete reg

{-------- Read Regex --------}

