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
, primes
, primesMask
, makeFactorizationArray
, makeFactorizer
, runFactorization
) where

import Data.Array
import qualified Data.Set as Set
import Data.List (group)
import Utils (cartProd)
import qualified Data.List.Ordered as Ordered
--import Debug.Trace

nmax = 1000000000
sqrtnmax1 = (floor $ sqrt $ fromIntegral nmax) + 1

guessPrimes = 2 : [3, 5 .. sqrtnmax1]

newtype Factorization = Factorization { getFactors :: [Integer] }

instance Show Factorization where
    show (Factorization f) = show f

factorize = factorizeNonCached

makeFactorizationArray :: Integer -> Array Integer Factorization
makeFactorizationArray nmax = arr
    where arr = listArray (2,nmax) [ Factorization (factorizeHelper i) | i <- [2..nmax] ]
          factorizeHelper i
--              | trace ("calling on " ++ show i) False = undefined
              | i<=2 = [i]
              | otherwise = let sqrtI = floor $ sqrt $ fromIntegral i
                                factors = takeWhile (<sqrtI) primes
                                smallestDivisor = filter (\x -> (mod i x) == 0) factors
                            in case smallestDivisor of
                                [] -> [i]
                                (x:xs) -> x : (getFactors $ arr ! (i `div` x))

data Factorizer = Factorizer { runFactorization :: (Integer -> Factorization) }

makeFactorizer :: Integer -> Factorizer
makeFactorizer nmax = Factorizer (\n -> arr ! n)
    where arr = makeFactorizationArray nmax

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
        (x:_) -> Just x
        where small = dropWhile (\x -> (mod n x) /= 0) $ takeWhile (<n) factors

isPrime x
        | x<2 = False
        | otherwise = case small of
           Nothing -> True
           Just _ -> False
           where small = smallestDivisor x $ takeWhile (<(ceiling $ sqrt $ fromIntegral x)) primes

isPrimeWithTableUpToN n x
        | x<2 = False
        | x>=n = error "prime table size exceeded"
        | otherwise = case small of
           Nothing -> True
           Just _ -> False
           where small = smallestDivisor x tableUpToN
                 tableUpToN = takeWhile (<=n) primes

primeTable = makePrimeTable nmax

makePrimeTable n = filter isPrime [2..n]

makePrimeArray :: Integer -> Array Integer Bool
makePrimeArray n = listArray (2,n) someBools
        where diffs = zipWith (-) (tail primes) primes
              ints = diffsToInts diffs
              someBools = take (fromIntegral n) $ (True : map (==1) ints)

diffsToInts [] = []
diffsToInts (x:xs) | x==0 = diffsToInts xs
                   | otherwise = x : diffsToInts ((x-1):xs)

--makePrimeArray n = listArray (2,n) $ [ primes `Ordered.has` p | p <- [2..n] ]

primes :: [Integer]
primes = 2 : oddprimes
  where 
    oddprimes = sieve [3,5..] 9 oddprimes
    sieve (x:xs) q ps@ ~(p:t)
      | x < q     = x : sieve xs q ps
      | otherwise =     sieve (xs `Ordered.minus` [q, q+2*p..]) (head t^2) t

primesMask = primesMask' primes 0
        where primesMask' (p:ps) last = (replicate (fromIntegral $ p-last-1) False) ++ (True : primesMask' ps p)

primeSet = Set.fromList primeTable

primeFactors n = zip factors powers
        where factors = map head groups
              powers = map length groups
              groups = (group $ factorize n primes)

divisors n = foldl cartProd [1] divisorList
        where divisorList = [ [ i^j | j <- [0..k] ] | (i, k) <- pFactors ]
              pFactors = primeFactors n

sumDivisors n = (sum $ divisors n) - n

isAbundant n = (sumDivisors n) > n

