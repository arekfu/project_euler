import Utils (isPerfectSquare)
import Data.Ratio
import Data.List
import ContinuedFractions
--import Primes
--import qualified Data.List.Ordered as Ordered

--factorsForX d y = primes `Ordered.minus` (Ordered.sort (factorsD ++ factorsY))
--        where factorsD = factorize d primes
--              factorsY = factorize primes

isSolution d (x,y) = (x^2 - d*y^2 == 1)

pairs :: Integer -> [(Integer, Integer)]
pairs d = [ (x,y) | n <- [1..],
            let app = nthApproximant n $ continuedFractionExpansionSqrt d,
            let x = numerator app, let y = denominator app ]

ds = filter (not . isPerfectSquare) [1..1000]

firstSolution :: Integer -> (Integer, Integer, Integer)
firstSolution d = (x,y,d)
        where (x,y) = head $ filter (isSolution d) (pairs d)

answer = sort $ map firstSolution ds

main = print answer
