# Newton-Algorithm and Midpoint Method for Intervals

created by Eva Richter, Matti Richter July 2018

 This repository features two classic algorithms on real functions modeled as haskell functions on Intervals with floating point numbers as bounds. The algorithms were taken from the book "Validated Numerics" by WarwickTucker 2011.
 
 In the folder Intervals, a type "Interval" and corresponding arithmetic functions are defined in such a way, that all possible rounding errors that could occur are accounted for, i.e. the output interval is guaranteed to contain the correct solution, if all input conditions are met. 
 
 The module Newton then implements the Newton method for intervals as described in the book, but unlike the version in the book, which sometimes uses the Interval extension of the function and sometimes uses the real function, this algorithm uses only the interval extension, since there usually will not be a precise implementation for the real function.
 
 The module MidpointMethod contains the other algorithm, which finds the minimum of a (continuous) function and the corresponding arguments by repeadtedly splitting the domain in half, computing upper and lower bounds for the values in each part of the domain and weeding out those, whose values are definitely bigger that the minimum.
 
 Both of these algorithms require an interval extension of a function as input. Since the use may not have an implementation for that there are the modules Polynomial and Expression, which can be used to created interval versions of functions by describing them abstractly. 
 
 In the module Polynomial, an interval extension of a polynomial can be created by simply giving the coefficients (as doubles). The user can also take the derivative of these polynomial, which is needed for the newton algorithm. As long as the coefficients of the polynomial are relatively simple, the derivative will be exactly correct (no rounding error occurs when mutliplying the coefficients by the powers), and the resulting functions are guaranteed to be interval extensions of the real ones.
 
 In the module Expression, more general continuous functions can be described such as sin, cos, exp in additions to powers and constants. These can then be concatenated, multiplied, divided or added to create more complicated functions. A derivative of such a function can also be taken. The resulting function will be a sort of interval version of the correct one, but will not guarantee to always contain teh correct solution, since sone of the more advanced functions to not safely round outwards. The module is still useful to test the Newton and Midpoint algorithms, though.
