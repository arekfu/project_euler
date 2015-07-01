module Utils
( cartProd
, factorial
, digitsToNumber
, numberToDigits
, allDifferentDigits
, ithPermutation
, generatePermutations
, ithSampleWithoutReplacement
, generateSamplesWithoutReplacement
, generateNumbers
, isPentagonal
, isHexagonal
, isPerfectSquare
)
where

import qualified Data.Set as Set
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
