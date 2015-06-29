module Utils
( cartProd
, factorial
, digitsToNumber
, numberToDigits
, allDifferentDigits
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
