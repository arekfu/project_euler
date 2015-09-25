module Primes
( primeTable
, makePrimeTable
, makePrimeArray
, guessPrimes
, primeSet
, smallestDivisor
, isPrime
, isPrimeWithTableUpToN
, divisors
, sumDivisors
, isAbundant
, primes
, primesMask
, makeFactorizationArray
, makeFactorizer
, runFactorization
, coprime
, getFactors
, phiFold
, Factorization()
) where

import Data.Array
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (group, groupBy, foldl')
import Utils (cartProd)
import qualified Data.List.Ordered as Ordered
import Data.Function (on)
--import Debug.Trace

nmax = 1000000000
sqrtnmax1 = (floor $ sqrt $ fromIntegral nmax) + 1

guessPrimes = 2 : [3, 5 .. sqrtnmax1]

newtype Factorization = Factorization { getFactors :: Map.Map Integer Integer }
    deriving (Show, Eq)

makeFactorizationArray :: Integer -> Array Integer Factorization
makeFactorizationArray nmax = arr
    where arr = listArray (2,nmax) [ Factorization (factorizeHelper i) | i <- [2..nmax] ]
          factorizeHelper i
--              | trace ("calling on " ++ show i) False = undefined
              | i<=2 = Map.singleton i 1
              | otherwise = case (smallestDivisor i primes) of
                                Nothing -> Map.singleton i 1
                                Just x -> Map.insertWith (+) x 1 (getFactors $ arr ! (i `div` x))

($*$) :: Factorization -> Factorization -> Factorization
infixl 7 $*$
f1 $*$ f2 = Factorization $ Map.unionWith (+) fac1 fac2
    where fac1 = getFactors f1
          fac2 = getFactors f2

data Factorizer = Factorizer { runFactorization :: (Integer -> Factorization) }

makeFactorizer :: Integer -> Factorizer
makeFactorizer nmax = Factorizer (\n -> arr ! n)
    where arr = makeFactorizationArray nmax

smallestDivisor :: Integer -> [Integer] -> Maybe Integer
smallestDivisor n factors = case small of
        [] -> Nothing
        (x:_) -> Just x
        where small = dropWhile (\x -> (mod n x) /= 0) $ takeWhile (<=sqrtN) factors
              sqrtN = floor $ sqrt $ fromIntegral n

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

divisors factorizer n = foldl cartProd [1] divisorList
        where divisorList = [ [ i^j | j <- [0..k] ] | (i, k) <- pFactors ]
              pFactors = Map.assocs $ getFactors $ runFactorization factorizer n

sumDivisors _ 0 = 0
sumDivisors _ 1 = 0
sumDivisors factorizer n = (sum $ divisors factorizer n) - n

isAbundant factorizer n = (sumDivisors factorizer n) > n

coprime :: Factorizer -> Integer -> Integer -> Bool
coprime f m n = not $ Map.null $ fm `Map.intersection` fn
    where fm = getFactors $ runFactorization f $ m
          fn = getFactors $ runFactorization f $ n

phiFold :: Factorization -> Integer
phiFold f = phiFold' $ getFactors f
    where phiFold' m = Map.foldlWithKey' accumulator 1 m
          accumulator acc prime power = acc * phiFactor
                where phiFactor = (prime-1) * prime^(power-1)
