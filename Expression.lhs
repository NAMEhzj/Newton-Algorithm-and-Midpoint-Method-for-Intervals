> {-# LANGUAGE GADTs #-}
> -- introduces an "expression" type, with wich one can construct differentiable functions
> -- 
> module Expression where
> import Intervals.IntervalType
> import Intervals.IntervalArithmetic
> import Intervals.IntervalProp 
>
>
>
>
>
> data Expression where
>         Const :: Double -> Expression
>         Pow :: Double -> Expression
>         Exp :: Expression
>         Sin :: Expression
>         Cos :: Expression
>         Log :: Expression
>         Neg :: Expression -> Expression
>         Comp :: Expression -> Expression -> Expression
>         Add :: Expression -> Expression -> Expression
>         Mul :: Expression -> Expression -> Expression
>         Div :: Expression -> Expression -> Expression deriving Show
> 
>
> 
> derivative :: Expression -> Expression
> derivative (Const _)             = Const 0
> derivative (Pow p)   | p == 0    = Const 0
>                      | otherwise = Mul (Const p) (Pow (p-1))
> derivative Exp                   = Exp
> derivative Sin                   = Cos
> derivative Cos                   = Neg Sin
> derivative Log                   = Pow (-1)
> derivative (Neg e)               = Neg (derivative e)
> derivative (Comp e1 e2)          = Mul (Comp (derivative e1) e2) (derivative e2)
> derivative (Add  e1 e2)          = Add (derivative e1) (derivative e2)
> derivative (Mul  e1 e2)          = Add (Mul (derivative e1) e2) (Mul e1 (derivative e2))
> derivative (Div  e1 e2)          = Div (Add (Mul (derivative e1) e2) (Neg (Mul e1 (derivative e2)))) (Comp (Pow 2) e2)
>
>
>
>
>
> eval :: Expression -> Interval -> Interval
> eval (Const c)    _ = double2Interval c
> eval (Pow p)      z = powI z p
> eval Exp          z = expI z
> eval Sin          z = sinI z
> eval Cos          z = cosI z
> eval Log          z = logI z
> eval (Neg e)      z = negate $ eval e z
> eval (Comp e1 e2) z = eval e1 $ eval e2 z
> eval (Add e1 e2)  z = add (eval e1 z) (eval e2 z)
> eval (Mul e1 e2)  z = mul (eval e1 z) (eval e2 z)
> eval (Div e1 e2)  z = simpleDiv (eval e1 z) (eval e2 z)
>
>
>
>
>
> example1 = Comp Exp (Pow 2)