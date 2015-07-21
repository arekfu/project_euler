import ContinuedFractions
import Utils

lengths = map (length . periodicPart . periodicContinuedFractionExpansionSqrt) [2..10000]

oddLengths = filter isOdd lengths

answer = length oddLengths

main = print answer
