module Primes
( primeTable
, factorize
, isPrime
, primeFactors
, divisors
, sumDivisors
, isAbundant
) where

import Data.Array
import Data.List (group)
import Utils (cartProd)

nmax = 1000000
sqrtnmax1 = (floor $ sqrt $ fromIntegral nmax) + 1

guessPrimes = 2 : [3, 5 .. sqrtnmax1]

factorize = factorizeNonCached

factorizeCached :: Integer -> [Integer]
factorizeCached n = arr ! n
        where arr = listArray (2,n) [ factorizeHelper i | i <- [2..n] ]
              factorizeHelper i
                      | i<=2 = [i]
                      | otherwise = let sqrti1 = (floor $ sqrt $ fromIntegral i) + 1
                                        factors = 2 : [3, 5 .. sqrti1]
                                        smallestDivisor = filter (\x -> (mod i x) == 0) factors
                                        in case smallestDivisor of
                                              [] -> [i]
                                              (x:xs) -> x : (arr ! (i `div` x))

factorizeNonCached :: Integer -> [Integer] -> [Integer]
factorizeNonCached n factors
        | n==1 = []
        | otherwise = let sqrtn1 = (floor $ sqrt $ fromIntegral n) + 1
                          smallestDivisor = filter (\x -> (mod n x) == 0) factors
                          in case smallestDivisor of
                                [] -> [n]
                                (x:xs) -> x : factorizeNonCached (n `div` x) factors

isPrime x
        | x<2 = False
        | otherwise = (length $ factorize x primeTable) <= 1

primeTable = filter isPrime [2..nmax]

primeFactors n = zip factors powers
        where factors = map head groups
              powers = map length groups
              groups = (group $ factorize n primeTable)

divisors n = foldl cartProd [1] divisorList
        where divisorList = [ [ i^j | j <- [0..k] ] | (i, k) <- pFactors ]
              pFactors = primeFactors n

sumDivisors n = (sum $ divisors n) - n

isAbundant n = (sumDivisors n) > n

