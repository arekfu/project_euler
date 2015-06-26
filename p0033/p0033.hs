import Utils
import Data.Ratio
import Data.List

rangeMin = 10
rangeMax = 99

simplifyFractionNaively :: (Integer, Integer) -> (Integer, Integer)
simplifyFractionNaively (num, den) = (simplNum, simplDen)
        where numDigits = numberToDigits num
              denDigits = numberToDigits den
              commonDigits = intersect numDigits denDigits
              simplNumDigits = numDigits \\ commonDigits
              simplDenDigits = denDigits \\ commonDigits
              simplNum = digitsToNumber simplNumDigits
              simplDen = digitsToNumber simplDenDigits

rationalFromPair (a, b) = a % b

pairs = [ (a, b) | a <- [rangeMin..rangeMax], b <- [a+1..rangeMax] ]

simplifiedPairs = map simplifyFractionNaively pairs

sanePairs = filter (\((a,b),(c,d)) -> c<10 && c>0 && d>0) $ zip pairs simplifiedPairs

validPairs = filter (\((a,b),(c,d)) -> (a % b) == rationalFromPair (c, d)) sanePairs

validNonTrivialPairs = filter (\((a,b),(c,d)) -> a /= c*10) validPairs

answer = foldl (*) (1%1) (map (rationalFromPair . fst) validNonTrivialPairs)
