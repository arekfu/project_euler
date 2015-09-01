import Data.List
import Data.Ratio
import Data.Function

main = print answer

closestTo f den = let denF = denominator f
                      numF = numerator f
                  in ((numF * den - 1) `div` denF) % den

closestToUpTo f denMax = map (closestTo f) [2..denMax]

minDiffClosestTo f denMax = minimumBy (compare `on` (f-)) $ closestToUpTo f denMax
answer = minDiffClosestTo (3%7) 1000000
