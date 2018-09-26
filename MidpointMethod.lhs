> -- this module contains the midpoint method for finding the minimum of a function using interval arithmetic.
> -- for details see Validated Numerics, by WarwickTucker 2011 page 87
>
> module MidpointMethod where
>
> import Intervals.IntervalType
> import Intervals.IntervalArithmetic
> import Intervals.IntervalProp 
> import Intervals.IntervalOps
> import Polynomial
> import Expression
>
> -- first helper function: splits  an interval into roughly two halves, it may not
> -- be exactly two halves, since there could be a rounding error in the calculation of the center
> -- but that does not matter here
>
> bisect :: Interval -> [Interval]
> bisect z@(IV a b) = [ IV a (cen z) , IV (cen z) b ]
>
> -- the algorithm always carries the output set as a list of intervals which are 
> -- iteratively created by bisecting over and over, so they will always be ordered 
> -- in the list. That makes it easy to just fuse touching intervals together for simplicity.
> -- clean does exactly that (empty and single lists stay the same, if two elements are there and touch, they get fused)
> clean :: [Interval] -> [Interval]
> clean [] = []
> clean (x : []) = [x]
> clean (x@(IV a b) : y@(IV c d) : zs) | b == c    = clean ((IV a d) : zs)
>                                      | otherwise = x : clean (y:zs) 
>
>
> -- the recursive function upon which the Midpoint Method is built:
> -- if all intervals are small enough, output the curret upper bound for the infimum
> -- and return a cleaned up version of the domain areas where it could occur
> -- otherwise, throw away intervals that contain no small enough values, get a new estimation for the
> -- infimum and bisect all the remaining intervals and recursively call.
>
> midPtRec :: (Interval -> Interval) -> (Double, [Interval]) -> (Double, [Interval])
> midPtRec f (inf , zs) | (maximum $ map len zs) < 0.0000001 = (inf, clean zs)
>                       | otherwise                        = midPtRec f (newInf, zs'')
>                                      where zs'' = concat $ map bisect zs'
>                                            newInf = min inf $ minimum $ map (rb.f.double2Interval.cen) zs'
>                                            zs'  = filter smallerThanInf zs
>                                            smallerThanInf z = lb (f z) <= inf
> 
>
> -- the main function: executes the first iteration of the algorithm and then calls the recursive version.
> -- (you could also just start with a one-element list and infinity as upper bound, but i don't like infinity as a Double)
> midpointMethod :: (Interval -> Interval) -> Interval -> (Double, [Interval])
> midpointMethod f z = midPtRec f (rb (f (double2Interval (cen z))) , bisect z) 
>                                            
>
>
>
>
>
