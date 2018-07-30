> -- this module gives a possibility to model Polynomials using just a list of coefficients 
> module Polynomial where
>
> import Intervals.IntervalType
> import Intervals.IntervalArithmetic
> import Intervals.IntervalProp 
> import Intervals.IntervalOps
>
>
> -- a polynomial is given by a list of coefficients, stating from the one for x^0 and then x^1 etc up to whatever 
> -- highest power one needs
> data Polynomial = Poly [Double]
>
> -- a function to get any coefficient from the polynomial, if the list doesnt specify it, it is assumed to be 0
> coefficient :: Int -> Polynomial -> Double
> coefficient _ (Poly []) = 0
> coefficient n (Poly (x:xs)) | x > 0     = coefficient (n-1) (Poly xs)
>                             | x == 0    = x
>                             | otherwise = 0   
>
>
>
> -- a formal derivative. just the power rule applied to all the a_k * x^k terms.
> -- i cheat a little bit and use double multiplication for "n*x", but in all our examples 
> -- this calculation will be exact since the coefficients will not use all of the precision that double provides, 
> -- so multiplying by an integer will not produce a rounding error.
> derivativeP :: Polynomial -> Polynomial 
> derivativeP (Poly [])     = Poly []
> derivativeP (Poly (_:xs)) = Poly (derivativeRec xs 1)
>             where derivativeRec [] _ = []
>                   derivativeRec (x:xs) n = (n*x):(derivativeRec xs (n+1)) 
>
>
> -- an evaluation function. It transforms the Polynomial into an interval extension 
> -- of the function described by it (calculating f(x) = a_0 + x*(a_1 + x*(a_2 + x*(...))) ) 
> toFunc :: Polynomial -> Interval -> Interval
> toFunc (Poly []) _       = num2Interval 0 0
> toFunc (Poly (c:[])) _   = num2Interval c c 
> toFunc (Poly (c:d:es)) z = add (num2Interval c c) $ mul z $ toFunc (Poly (d:es)) z
>