import Data.List
import Data.Ratio
import Data.Function

main = print answer

closestTo f den = let denF = denominator f
                      numF = numerator f
                  in ((numF * den) `div` denF) % den

closestUpTo f denMax = filter (/= 3%7) $ map (closestTo f) [2..denMax]

minDiffClosestTo f denMax = minimumBy (compare `on` (f-)) $ closestUpTo f denMax
answer = minDiffClosestTo (3%7) 1000000
