module Primes
( primeTable
, makePrimeTable
, makePrimeArray
, guessPrimes
, primeSet
, factorize
, smallestDivisor
, isPrime
, isPrimeWithTableUpToN
, primeFactors
, divisors
, sumDivisors
, isAbundant
) where

import Data.Array
import qualified Data.Set as Set
import Data.List (group)
import Utils (cartProd)

nmax = 1000000000
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
        | otherwise = let smallest = smallestDivisor n factors
                          in case smallest of
                                Nothing -> [n]
                                Just x -> x : factorizeNonCached (n `div` x) factors

smallestDivisor :: Integer -> [Integer] -> Maybe Integer
smallestDivisor n factors = case small of
        [] -> Nothing
        (x:xs) -> Just x
        where small = dropWhile (\x -> (mod n x) /= 0) $ takeWhile (<n) factors

isPrime x
        | x<2 = False
        | otherwise = case small of
           Nothing -> True
           Just _ -> False
           where small = smallestDivisor x guessPrimes

isPrimeWithTableUpToN n x
        | x<2 = False
        | x>=n = error "prime table size exceeded"
        | otherwise = case small of
           Nothing -> True
           Just _ -> False
           where small = smallestDivisor x tableUpToN
                 tableUpToN = takeWhile (<=n) primeTable

primeTable = makePrimeTable nmax

makePrimeTable n = filter isPrime [2..n]

makePrimeArray n = listArray (2,n) $ [ isPrime i | i <- [2..n] ]

primeSet = Set.fromList primeTable

primeFactors n = zip factors powers
        where factors = map head groups
              powers = map length groups
              groups = (group $ factorize n primeTable)

divisors n = foldl cartProd [1] divisorList
        where divisorList = [ [ i^j | j <- [0..k] ] | (i, k) <- pFactors ]
              pFactors = primeFactors n

sumDivisors n = (sum $ divisors n) - n

isAbundant n = (sumDivisors n) > n

