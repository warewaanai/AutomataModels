module KleeneAlgebra (
    Kleene,
    KleeneMatrix
) where

data Kleene a = Add a a | Mul a a | Star a a deriving (Eq)
data KleeneMatrix a = Mat [[a]] deriving (Eq, Show)


