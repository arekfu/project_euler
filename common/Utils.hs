module Utils
( cartProd
, factorial
, digitsToNumber
, numberToDigits
, numberOfDigits
, allDifferentDigits
, ithPermutation
, generatePermutations
, generateDigitPermutations
, ithSampleWithoutReplacement
, generateSamplesWithoutReplacement
, generateNumbers
, isPentagonal
, isHexagonal
, isPerfectSquare
, concatNumbers
, concatListOfNumbers
, solve2ndDegree
, isOdd
, isEven
, isKthPower
, digitalSum
, firstRepeatingIndex
)
where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace

cartProd l1 l2 = [ x1 * x2 | x1 <- l1, x2 <- l2 ]

factorial :: Integer -> Integer
factorial n = product [1..n]

digitsToNumber :: (Num a) => [a] -> a
digitsToNumber digs = foldl (\x -> \y -> x*10+y) 0 digs

numberToDigits :: (Integral a) => a -> [a]
numberToDigits = reverse . numberToDigitsBackwards
        where numberToDigitsBackwards n
                | n<10 = [n]
                | otherwise = d : numberToDigitsBackwards (n `div` 10)
                where d = n `mod` 10

numberOfDigits :: (Integral a) => a -> a
numberOfDigits n = fromIntegral $ length $ numberToDigits n

allDifferentDigits n = nDigits==nDifferentDigits
        where digits = numberToDigits n
              nDigits = length digits
              nDifferentDigits = length $ Set.fromList $ numberToDigits n

ithPermutation :: Integer -> [a] -> [a]
ithPermutation rank alphabet
--        | trace (show index ++ " " ++ show factn1 ++ " ") False = undefined
        | rank > (fromIntegral n)*factn1 = error "rank must be <= (length alphabet)!"
        | n==1 = alphabet
        | otherwise = firstChar : ithPermutation (rank `mod` factn1) remainingAlphabet
        where n = length alphabet
              factn1 = factorial $ fromIntegral (n-1)
              index = fromIntegral $ rank `div` factn1
              (firstChar, remainingAlphabet) = extractListElementAt alphabet index

extractListElementAt :: [a] -> Int -> (a, [a])
extractListElementAt s i = (char, rest)
        where char = s !! i
              rest = (take i s) ++ (drop (i+1) s)

generateNumbers digits = map digitsToNumber $ generatePermutations digits

generatePermutations list = map (\n -> ithPermutation n list) [0..factn1]
        where factn1 = (factorial $ fromIntegral $ length list) - 1

ithSampleWithoutReplacement :: Integer -> Integer -> [a] -> [a]
ithSampleWithoutReplacement rank k alphabet
--        | trace ("+++" ++ show rank ++ " " ++ show nSamples ++ " " ++ show nSamples1 ++ "+++") False = undefined
        | rank>=nSamples = error "rank must be <= n! / (n-k)!"
        | k>n = error "sample size must be <= (length alphabet)!"
        | k==0 = []
        | otherwise = firstChar : ithSampleWithoutReplacement (rank `mod` nSamples1) (k-1) remainingAlphabet
        where n = fromIntegral $ length alphabet
              nSamples = nSamplesWithoutReplacement n k
              nSamples1 = nSamplesWithoutReplacement (n-1) (k-1)
              index = fromIntegral $ rank `div` nSamples1
              (firstChar, remainingAlphabet) = extractListElementAt alphabet index

generateSamplesWithoutReplacement :: Integer -> [a] -> [[a]]
generateSamplesWithoutReplacement k alphabet = map (\rank -> ithSampleWithoutReplacement rank k alphabet) [0..nSamples1]
        where nSamples1 = (nSamplesWithoutReplacement n k) - 1
              n = fromIntegral $ length alphabet

nSamplesWithoutReplacement :: Integer -> Integer -> Integer
nSamplesWithoutReplacement n k = product [(n-k+1)..n]

isPerfectSquare :: Integer -> Bool
isPerfectSquare n = (round $ sqrt $ fromIntegral n)^2 == n

isPentagonal :: Integer -> Bool
isPentagonal n = isPerfectSquare discriminant && (1 + (round $ sqrt $ fromIntegral discriminant)) `mod` 6 == 0
        where discriminant = 1 + 24*n

isHexagonal :: Integer -> Bool
isHexagonal n = isPerfectSquare discriminant && (1 + (round $ sqrt $ fromIntegral discriminant)) `mod` 4 == 0
        where discriminant = 1 + 8*n

generateDigitPermutations :: Integer -> [Integer]
generateDigitPermutations n = map digitsToNumber $ generatePermutations $ numberToDigits n

concatListOfNumbers :: [Integer] -> Integer
concatListOfNumbers [] = 0
concatListOfNumbers (n:ns) = n + factor*(concatListOfNumbers ns)
        where factor = head $ dropWhile (<=n) [ 10^k | k <- [0..]]

concatNumbers :: Integer -> Integer -> Integer
concatNumbers m n = n + factor*m
        where factor = head $ dropWhile (<=n) [ 10^k | k <- [0..]]

solve2ndDegree :: (Double, Double, Double) -> (Double, Double)
solve2ndDegree (c,b,a) = ((-d)-sqrtDiscrOver2a, (-d)+sqrtDiscrOver2a)
        where d = b / (2.0 * a)
              discr = b**2 - 4.0 * a * c
              sqrtDiscrOver2a = (sqrt discr) / (2.0 * a)

isOdd n = n `mod` 2 /= 0
isEven n = n `mod` 2 == 0

isKthPower :: (Integral a, Integral b) => a -> b -> Bool
isKthPower k n = (round kthRoot)^k == n
        where rk = fromIntegral k
              rn = fromIntegral n
              kthRoot = (rn**((1.0::Double)/rk)) :: Double

digitalSum n | n<10 = n
             | otherwise = digitalSum $ sum $ numberToDigits n

firstRepeatingIndex :: (Ord a) => [a] -> (Int, Int)
firstRepeatingIndex list = firstRepeatingIndex' list 0 Map.empty
        where firstRepeatingIndex' [] index _ = (index, index)
              firstRepeatingIndex' (x:xs) index m | x `Map.member` m = (m Map.! x, index)
                                                  | otherwise = firstRepeatingIndex' xs (index+1) (Map.insert x index m)

