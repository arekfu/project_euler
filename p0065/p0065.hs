import ContinuedFractions
import Utils
import Data.Ratio

eExpansion = toCFExpansion $ 2:period
        where period = concatMap (\n -> [1, 2*n, 1]) [1..]

hundredthApproximant = nthApproximant 100 eExpansion

answer = sum $ numberToDigits $ numerator hundredthApproximant

main = print answer

