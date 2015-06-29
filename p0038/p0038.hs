import qualified Data.Set as Set
import Utils

productKs kmax = [ [ k*n | k <- [1..kmax] ] | n <- candidates ]
        where candidates = filter allDifferentDigitsNo0 [2..nmax]
              nmax = 10^((9 `div` kmax) + 1) - 1

allDifferentDigitsNo0 n = nDigits==nDifferentDigits && (not (0 `elem` digits))
        where digits = numberToDigits n
              nDigits = length digits
              nDifferentDigits = length $ Set.fromList $ numberToDigits n

concatProducts l = concat $ map numberToDigits l

concatenatedProductKs kmax = map concatProducts $ productKs kmax

validProduct l = (not (0 `elem` l)) && (length $ l) == 9 && (length $ Set.fromList $ l) == 9

validProductKs kmax = filter validProduct $ concatenatedProductKs kmax

validProductKsAsInts kmax = map digitsToNumber $ validProductKs kmax

allValidProducts = concat $ map validProductKsAsInts [2..5]

maxValidProduct = maximum allValidProducts
