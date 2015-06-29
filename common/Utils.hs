module Utils
( cartProd
, factorial
, digitsToNumber
, numberToDigits
, allDifferentDigits
, ithPermutation
)
where

import qualified Data.Set as Set

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
              (firstChar, remainingAlphabet) = splitListAt alphabet index

splitListAt :: [a] -> Int -> (a, [a])
splitListAt s i = (char, rest)
        where char = s !! i
              rest = (take i s) ++ (drop (i+1) s)

