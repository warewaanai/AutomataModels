module UntypedLambdaCalculus(
    Var,
    Lambda
) where

type Var = String
data Lambda = App Lambda Lambda | Abs Var Lambda | Var deriving (Eq)
