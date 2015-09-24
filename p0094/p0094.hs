import Utils (isPerfectSquare, zipMap)
import Primes

--b^2 = (a+1)^2-a^2/4 = 3a^2/4+2a+1 = (a+2)(3a+2)/4, p = a+2(a+1) = 3a+2, A = ab/2
--b^2 = (a-1)^2-a^2/4 = 3a^2/4-2a+1 = (a-2)(3a-2)/4, p = a+2(a-1) = 3a-2, A = ab/2

pMax = 10^9

someAsPlusH = filter (\(a, h) -> isPerfectSquare h) $ zipMap (\a -> (a+2)*(3*a+2)) [2,4..]
someAsMinusH = filter (\(a, h) -> isPerfectSquare h) $ zipMap (\a -> (a-2)*(3*a-2)) [4,6..]
someAsPlus = map fst someAsPlusH
someAsMinus = map fst someAsMinusH

fer = makeFactorizer 700000

factorsPlus = map (runFactorization fer . (\a -> a+1) . fst) someAsPlusH
factorsMinus = map (runFactorization fer . (\a -> a-1) . fst) someAsMinusH

asPlus = (someAsPlus !! 0) : (someAsPlus !! 1) : (someAsPlus !! 2) : (zipWith3 (\a b c -> 15*a - 15*b + c) (tail $ tail asPlus) (tail asPlus) asPlus)
asMinus = (someAsMinus !! 0) : (someAsMinus !! 1) : (someAsMinus !! 2) : (zipWith3 (\a b c -> 15*a - 15*b + c) (tail $ tail asMinus) (tail asMinus) asMinus)

smallestAsPlus = takeWhile (<=(pMax-2) `div` 3) asPlus
smallestAsMinus = takeWhile (<=(pMax+2) `div` 3) asMinus

perimPlus a = 3*a+2
perimMinus a = 3*a-2

sumPerims = sum (map perimPlus smallestAsPlus) + sum (map perimMinus smallestAsMinus)

answer = sumPerims
