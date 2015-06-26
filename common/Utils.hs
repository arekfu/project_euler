module Utils
( cartProd
, factorial
)
where

cartProd l1 l2 = [ x1 * x2 | x1 <- l1, x2 <- l2 ]

factorial :: Integer -> Integer
factorial n
        | n<=1 = 1
        | otherwise = n * (factorial (n-1))
