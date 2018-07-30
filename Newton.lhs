> -- this module implements the newton method for finding zeros of a polynomial using intervals 
> -- (both, actual "big" intervals in the algorithm and "small" intervalls to estimate the error)
> module Newton where
>
> import Intervals.IntervalType
> import Intervals.IntervalArithmetic
> import Intervals.IntervalProp 
> import Intervals.IntervalOps
> import Polynomial
> import Expression
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
> -- important: for the following functions the second input is supposed to be the derivative of the first
> -- if that is not the case the algorithm will most like ley produce nonsense or nothing at all
>
> -- one step of the newton algorithm (see Warwick Tucker ...)
> newtonStep :: Interval -> (Interval -> Interval) -> (Interval -> Interval) -> Maybe Interval
> newtonStep x f f' = meet x $ sub m (simpleDiv (f m) (f' x))
>                       where m = double2Interval(cen x)
>
>
> -- iteration of the one-step version, stops if either the the step returns nothing or if the estimation doesnt get any better
> -- since the arithmetic in the step always increases the interval a bit there will be a point where that increase is bigger than the 
> -- improvement of the precision given by the algorithm, so the function will terminate.
> -- if the inputs were correct it is then assured that the resulting interval contains both the exact real solution (limit) as well as the double implementation of
> -- the algorithm (limit) (if implemented like this one). The book by Tucker then proves that the real solution actually finds the correct zero, if there is one.
> newton :: Interval -> (Interval -> Interval) -> (Interval -> Interval) -> Maybe Interval
> newton x f f' = newtonIterate (newtonStep x f f') f f' (len x)
>                    where newtonIterate Nothing f f' _ = Nothing
>                          newtonIterate (Just x) f f' l | len x == l = Just x
>                                                        | otherwise  = newtonIterate (newtonStep x f f') f f' (len x)
>
>
> -- here the a special case for polynomials (see respective module)
> -- with this i can assure that the second input function actually is the derivative of the first
> polyNewton :: Interval -> Polynomial -> Maybe Interval
> polyNewton x p = newton x (toFunc p) (toFunc (derivativeP p))
>
>
> exprNewton :: Interval -> Expression -> Maybe Interval
> exprNewton x e = newton x (eval e) (eval (derivative e))