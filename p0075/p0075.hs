import Data.List

lMax = 1500000

isPythagorean (a,b,c) = a^2+b^2==c^2

perimeter (a,b,c) = a+b+c

daughters (a,b,c) = filter (\ t -> perimeter t<=lMax) [ d1 (a,b,c), d2 (a,b,c), d3 (a,b,c) ]

d1 (a,b,c) = (  a -2*b+2*c,   2*a -b+2*c,   2*a -2*b+3*c)
d2 (a,b,c) = (  a +2*b+2*c,   2*a +b+2*c,   2*a +2*b+3*c)
d3 (a,b,c) = ((-a)+2*b+2*c, (-2*a)+b+2*c, (-2*a)+2*b+3*c)

allDaughters t = ds ++ concatMap allDaughters ds
    where ds = daughters t

tree = (3,4,5) : allDaughters (3,4,5)

allTriples = concatMap (\ (a,b,c) -> map (\ n -> (a*n, b*n, c*n)) [1..lMax `div` perimeter (a,b,c)]) tree

sortedPerimeters = sort $ map perimeter allTriples

answer = length $ filter (\l -> length l==1) $ group sortedPerimeters
