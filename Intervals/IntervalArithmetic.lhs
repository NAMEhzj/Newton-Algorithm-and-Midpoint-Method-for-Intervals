> module Intervals.IntervalArithmetic where
>
> import Numeric.IEEE
>
> import Intervals.IntervalType
> import Intervals.IntervalProp
> import Intervals.IntervalOps
> import Intervals.RoundedArithmetic
> 
>
> {- since NaN compares with every Double to False, max NaN x =NaN but max x NaN = x, that is a problem;
>    to resolve it the function maxIEEE gives more intutive
>    results as it ignores NaN
>  -}
> 
> maxIEEE ::Double -> Double -> Double
> maxIEEE x y | isNaN x || isNaN y  = infinity
>             | otherwise           = max x y
> minIEEE ::Double -> Double -> Double
> minIEEE x y | isNaN x || isNaN y  = -infinity
>             | otherwise           = min x y
>
> {- implementation of arithmetic operations +,-,*,/ for intervals using the
>    directed rounded operations add_u,etc.
>  -}
>
> add :: Interval -> Interval -> Interval
> add x y = num2Interval (add_d (lb x)(lb y)) (add_u (rb x)(rb y))
>
> sub :: Interval -> Interval -> Interval
> sub x y = num2Interval (sub_d (lb x)(rb y)) (sub_u (rb x)(lb y))
>
> leftIntList :: Interval -> Interval -> [Double]
> leftIntList x y =( mul_d (lb x)(lb y)): ((mul_d (lb x)(rb y)):
>                  ((mul_d (rb x)(lb y)):(( mul_d (rb x)(rb y)):[])))
>
> rightIntList :: Interval -> Interval -> [Double]
> rightIntList x y =( mul_u (lb x)(lb y)): ((mul_u (lb x)(rb y)):
>                   ((mul_u (rb x)(lb y)):(( mul_u (rb x)(rb y)):[])))
>
>
> -- nan cannot serve as the value of min/max on the empty list
>
> mul :: Interval -> Interval -> Interval
> mul x y = num2Interval (foldr (minIEEE) (infinity) (leftIntList x y))
>                      (foldr (maxIEEE)(-infinity) (rightIntList x y))
> 
>
> pow :: Interval -> Int -> Interval
> pow z n | n > 1  = mul z (pow z (n-1))
>         | n == 1 = z
>         | n == 0 = double2Interval 1
>
> -- the following functions are NOT interval extensions of their real counterparts,
> -- i.e. they do not account for all rounding errors. 
>
>
>
> powI :: Interval -> Double -> Interval
> powI z p | p > 1  = mul z (powI z (p - 1))
>          | p == 1 = z
>          | p == 0 = double2Interval 1
>          | p > 0  = let lbn = min ((lb z) ** p) ((rb z) ** p)
>                         rbn = max ((lb z) ** p) ((rb z) ** p) 
>                      in num2Interval lbn rbn
>          | otherwise = simpleDiv (powI z (p + 1)) z
>
>
>
> expI :: Interval -> Interval
> expI z = num2Interval (exp (lb z)) (exp (rb z)) 
>
> logI :: Interval -> Interval
> logI z = num2Interval (log (lb z)) (log (rb z))
>
>
>
>
> sinI :: Interval -> Interval
> sinI z | (lb z) < 0     = sinI $ num2Interval ((lb z) + 2*pi) ((rb z) + 2*pi)
>        | (lb z) >= 2*pi = sinI $ num2Interval ((lb z) - 2*pi) ((rb z) - 2*pi)
>        | (lb z) >= pi   = negate $ sinI $ num2Interval ((lb z) - pi) ((rb z) - pi)
>        | ((lb z) < pi/2) && ((rb z) < pi/2)   = num2Interval (sin (lb z)) (sin (rb z))
>        | ((lb z) < pi/2) && ((rb z) < 3*pi/2) = num2Interval (min (sin (lb z)) (sin (rb z))) 1
>        | ((lb z) < pi/2)                      = num2Interval (-1) 1
>        |                    ((rb z) < 3*pi/2) = num2Interval (sin (rb z)) (sin (lb z))
>        |                    ((rb z) < 5*pi/2) = num2Interval (-1) (max (sin (lb z)) (sin (rb z)))
>        | otherwise                            = num2Interval (-1) 1
> 
>
> cosI :: Interval -> Interval
> cosI z = sinI $ num2Interval ((lb z) + pi/2) ((rb z) + pi/2)
>
>
>
>
> -- from here on its all clean again:
>
> -- division where divisor does not contain zeros, result is always a single interval,
> -- the exception of dividing by zero is handled by the control.exception module,
> -- divI and divI2 give as a result (-Infinity, Infinity) if either of the bounds of z or z' equals nan
> 
> simpleDiv :: Interval -> Interval -> Interval
> simpleDiv z z' | not (contZ z') = mul z (num2Interval (div_d 1 d) ( div_u 1 c))
>                            where c = lb z'
>                                  d = rb z'
>
> 
> divI ::  Interval -> Interval -> [Interval]
> divI z z' 
>   | not (contZ z')
>        = [mul z (num2Interval (div_d 1 d) ( div_u 1 c))]
>   |contZ z && contZ z'
>        = [num2Interval (-infinity)(infinity)]
>   | isIntN z && isIntNZ z'
>        = [num2Interval (div_d b c) infinity]
>   | isIntN z && isIntM z'
>        = [(num2Interval (-infinity)(div_u b d)), (num2Interval (div_d b c) infinity)]
>   | isIntN z && isIntPZ z'
>        = [num2Interval (-infinity) (div_u b d)]
>   | isIntP z &&  isIntNZ z'
>        = [num2Interval (-infinity) (div_u a c)]
>   | isIntP z && isIntM z'
>        = [(num2Interval (-infinity)(div_u a c)), (num2Interval (div_d a d) infinity)]
>   | isIntP z && isIntPZ z'
>        = [num2Interval (div_d a d) (infinity)]
>   | otherwise = []
>   where a = lb z
>         b = rb z
>         c = lb z'
>         d = rb z'
>
> -- the multiplicative inverse of an interval, s.t. invInt(infInt z) = z unless isIntZ z(after clean up)
>
> invInt :: Interval -> [Interval]
> invInt z | isIntZ z  = []
>          | b == 0    = [num2Interval (-infinity) (div_u 1 a)]
>          | a == 0    = [num2Interval (div_d 1 b) infinity]
>          | isIntM z  = invInt (num2Interval a 0) ++ invInt (num2Interval 0 b)
>          | otherwise = [num2Interval (div_d 1 b) (div_u 1 a)]
>           where a = lb z
>                 b = rb z
> 
> -- interval division based on the inverse function, gives the same results as divI
> 
> divI2 ::  Interval -> Interval -> [Interval]
> divI2 z z' | contZ z' && contZ z  = [num2Interval (-infinity) infinity]
>            | otherwise            = map (mul z) (invInt z')
>
> testlist = [(num2Interval (-1) 1),(num2Interval 0 1), (num2Interval (-infinity)(-1)), (num2Interval (-1) infinity),
>             (num2Interval 0 0),(num2Interval (-3) (-1))]
>
> testdiv = [ divI2 (testlist !! i )(testlist !! j )| i <- [0..5],j<- [0..5]]
>
> testdivI = [ divI (testlist !! i )(testlist !! j )| i <- [0..5],j<- [0..5]]
> 
> sqrtI :: Interval -> Interval
> sqrtI z
>     = num2Interval (sqrt (lb z)) (sqrt (rb z))
>   
>
> instance Num Interval where
>  negate z    = num2Interval (- (rb z))(- (lb z))
>
>  z + z' = add z z'
> 
>  z * z' = mul z z'
> 
>  abs z
>   | isIntP z
>     = z
>   | isIntM z
>     = num2Interval 0 (len z)
>   | isIntN z
>     = negate z
>   | isIntZ z
>     = num2Interval 0 0
>   | otherwise
>     = num2Interval nan nan
>
>  signum z
>   | isIntP z
>     = num2Interval 1 1
>   | isIntN z
>     = num2Interval (-1) (-1)
>   | isIntZ z
>     = num2Interval 0 0
>   | isIntM z
>     = num2Interval ((lb z)/(len z))((rb z)/(len z)) 
>   | otherwise
>     = num2Interval nan nan
> 
>  fromInteger k = num2Interval (fromInteger k) (fromInteger k)