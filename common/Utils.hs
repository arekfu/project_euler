module Utils
( cartProd
, factorial
, digitsToNumber
, numberToDigits
)
where

cartProd l1 l2 = [ x1 * x2 | x1 <- l1, x2 <- l2 ]

factorial :: Integer -> Integer
factorial n
        | n<=1 = 1
        | otherwise = n * (factorial (n-1))

digitsToNumber :: (Num a) => [a] -> a
digitsToNumber digs = foldl (\x -> \y -> x*10+y) 0 digs

numberToDigits :: (Integral a) => a -> [a]
numberToDigits = reverse . numberToDigitsBackwards
        where numberToDigitsBackwards n
                | n<10 = [n]
                | otherwise = d : numberToDigitsBackwards (n `div` 10)
                where d = n `mod` 10
