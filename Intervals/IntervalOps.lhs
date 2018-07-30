> module Intervals.IntervalOps where
> 
> import Intervals.IntervalType
>
> {- len measures the length of an interval, is NaN for the empty interval
>    and infinity for[-infinity,infinity]
>  -}
> len :: Interval -> Double
> len z =  rb z - lb z
>
>
> {- cen gives the value of the center of the interval -}
> cen :: Interval -> Double
> cen z = (rb z + lb z)/2
>
> {- dist gives the distance of two intervals-}
>
> dist :: Interval -> Interval -> Double
> dist z z' = max (lb z - rb z') (lb z'- rb z)
>
