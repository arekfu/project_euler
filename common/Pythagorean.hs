module Pythagorean
( triples
, allTriples
) where

import Data.List

daughters :: Integral a => ((a,a,a) -> Bool) -> (a, a, a) -> [(a, a, a)]
daughters pred (a,b,c) = filter pred $ map ($ (a,b,c)) [ d1, d2, d3 ]

canonical (a,b,c) = if a <= b then (a,b,c) else (b,a,c)

d1 (a,b,c) = (  a -2*b+2*c,   2*a -b+2*c,   2*a -2*b+3*c)
d2 (a,b,c) = (  a +2*b+2*c,   2*a +b+2*c,   2*a +2*b+3*c)
d3 (a,b,c) = ((-a)+2*b+2*c, (-2*a)+b+2*c, (-2*a)+2*b+3*c)

allDaughters :: Integral a => ((a, a, a) -> Bool) -> (a, a, a) -> [(a, a, a)]
allDaughters pred t = (map canonical ds) ++ concatMap (allDaughters pred) ds
    where ds = daughters pred t

triples pred = (3,4,5) : allDaughters pred (3,4,5)

allTriples pred = concatMap (\ (a,b,c) -> takeWhile pred $ map (\ n -> (a*n, b*n, c*n)) [1..]) tree
    where tree = triples pred
