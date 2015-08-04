import Data.Ratio
import Data.List.Ordered (minus)
import ContinuedFractions

toDecimalExpansion :: Rational -> [Integer]
toDecimalExpansion r = let num = numerator r
                           den = denominator r
                           ratio = num `div` den
                           rest = num `mod` den
                       in ratio : toDecimalExpansion ((10 * rest) % den)

nonPerfectSquares = [1..100] `minus` (map (^2) [1..10])

minimalKExpansion k n = expandSqrtEpsilon (1 % (10^(k+1))) n

firstKDigitsSqrt k n = sum $ take k $ toDecimalExpansion $ approximant $ minimalKExpansion k n

answer = sum $ map (firstKDigitsSqrt 100) nonPerfectSquares
