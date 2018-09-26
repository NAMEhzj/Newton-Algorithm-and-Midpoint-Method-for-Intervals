> -- this module implements the newton method for finding zeros of a differentiable function using intervals 
> -- (both, actual "big" intervals for the algorithm, see Validated Numerics by Warwick Tucker, 
> -- and "small" intervalls to estimate the error)

> module Newton where
>
> import Intervals.IntervalType
> import Intervals.IntervalArithmetic
> import Intervals.IntervalProp 
> import Intervals.IntervalOps
> import Polynomial
> import Expression
>
>
> -- an intersection of two intervals, returns nothing if empty, the resulting interval if non-empty.
> meet :: Interval -> Interval -> Maybe Interval
> meet z z' | (b >= a' && a <= b') = Just (num2Interval (max a a')(min b b'))
>           | otherwise            = Nothing
>                where a = lb z
>                      b = rb z
>                      a' = lb z'
>                      b' = rb z' 
>
>
> -- important: For the following functions the second function input is supposed to be the derivative of the first.
> -- If that is not the case, the algorithm will most likeley produce nonsense or nothing at all
>
> -- one step of the newton algorithm (see Warwick Tucker: Validated Numerics, 2011)
> newtonStep :: Interval -> (Interval -> Interval) -> (Interval -> Interval) -> Maybe Interval
> newtonStep x f f' = meet x $ sub m (simpleDiv (f m) (f' x))
>                       where m = double2Interval(cen x)
>
>
> -- iteration of the one-step version, stops if either the the step returns nothing or if the estimation doesn't get any better
> -- since the arithmetic in the step always increases the interval a bit there will be a point where that increase is bigger than the 
> -- improvement of the precision given by the algorithm, so the function will terminate.
> -- if the inputs were correct it is then assured that the resulting interval contains both the exact real solution (limit), as well as the double implementation of
> -- the algorithm (limit) (if implemented like this one). The book by Tucker then proves that the real solution actually finds the correct zero, if there is one.
> newton :: Interval -> (Interval -> Interval) -> (Interval -> Interval) -> Maybe Interval
> newton x f f' = newtonIterate (newtonStep x f f') f f' (len x)
>                    where newtonIterate Nothing  f f' _              = Nothing
>                          newtonIterate (Just x) f f' l | len x == l = Just x
>                                                        | otherwise  = newtonIterate (newtonStep x f f') f f' (len x)
> -- note, that the derivative function cannot have a zero in the given starting inerval, otherwise the function will 
> -- most likeley produce nonsense!
>
>
> -- here the special case for polynomials (see respective module)
> -- with this i can assure that the second input function actually is the derivative of the first
> -- (up to rounding errors of the coefficients. If they are simple these will not occur, though)
> polyNewton :: Interval -> Polynomial -> Maybe Interval
> polyNewton x p = newton x (toFunc p) (toFunc (derivativeP p))
>
> -- another special case for "expressions" (see resp. module for more information)
> -- same thing asin polynomials, except that the evaluation does not produce interval extensions, so this
> -- version might lie a little bit.
> exprNewton :: Interval -> Expression -> Maybe Interval
> exprNewton x e = newton x (eval e) (eval (derivative e))
>
> -- when in doubt one can always implement his/her own Interval-extensions and derivatives and
> -- then use them with the "newton" function, that way all the properties are secured.
