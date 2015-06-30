import Primes
import Utils
import Data.List

theDigits :: [Integer]
theDigits = [0..9]

suffixes = filter (\l -> (digitsToNumber l) `mod` 17 == 0) $ generateSamplesWithoutReplacement 3 theDigits

candidates = map digitsToNumber $ concat $ map completeList suffixes

completeList l = zipWith (++) rest (repeat l)
        where restDigits = theDigits \\ l
              rest = generatePermutations restDigits

triplet i l = take 3 $ drop i l

triplets l = map digitsToNumber $ map (\i -> triplet i l) [1,2,3,4,5,6,7]

areAllTripletsDivisible n = and $ map (==0) $ zipWith mod (triplets $ numberToDigits n) [2, 3, 5, 7, 11, 13, 17]

allNums = filter areAllTripletsDivisible candidates

answer = sum allNums
