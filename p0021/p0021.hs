import Data.Array
import Data.List (group)
import Debug.Trace

factorize :: Integer -> [Integer]
factorize n = arr ! n
        where arr = listArray (2,n) [ factorizeHelper i | i <- [2..n] ]
              factorizeHelper i
                      | i<=2 = [i]
                      | otherwise = let sqrti1 = (floor $ sqrt $ fromIntegral i) + 1
                                        factors = 2 : [3, 5 .. sqrti1]
                                        smallestDivisor = filter (\x -> (mod i x) == 0) factors
                                        in case smallestDivisor of
                                              [] -> [i]
                                              (x:xs) -> x : (arr ! (i `div` x))

factorizeNonCached :: Integer -> [Integer]
factorizeNonCached n
        | n==1 = []
        | otherwise = let sqrtn1 = (floor $ sqrt $ fromIntegral n) + 1
                          factors = 2 : [3, 5 .. sqrtn1]
                          smallestDivisor = filter (\x -> (mod n x) == 0) factors
                          in case smallestDivisor of
                                [] -> [n]
                                (x:xs) -> x : factorizeNonCached (n `div` x)

primeFactors n = zip factors powers
        where factors = map head groups
              powers = map length groups
              groups = (group $ factorizeNonCached n)

factors n
--        | trace (show n ++ " " ++ show pFactors) False = undefined
        | otherwise = [ [ i^j | j <- [0..k] ] | (i, k) <- pFactors ]
        where pFactors = primeFactors n

cartProd l1 l2 = [ x1 * x2 | x1 <- l1, x2 <- l2 ]

divisors n = foldl cartProd [1] $ factors n

sumDivisors n = (sum $ divisors n) - n

nmax = 9999

sumAmicable
--        | trace (show sums) False = undefined
        | otherwise = sum amicables
        where amicables = [ i + j | i <- [2..nmax], j <- [i+1..nmax], sumsArr!i == j && sumsArr!j == i ]
              sums = map sumDivisors [1..nmax]
              sumsArr = listArray (1,nmax) sums

