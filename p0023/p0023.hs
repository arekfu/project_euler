import Data.Array
import Data.List (group)
import Debug.Trace
import qualified Data.Set as Set

nmax = 28123
sqrtnmax1 = (floor $ sqrt $ fromIntegral nmax) + 1

guessPrimes = 2 : [3, 5 .. sqrtnmax1]

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

factorizeNonCached :: Integer -> [Integer] -> [Integer]
factorizeNonCached n factors
        | n==1 = []
        | otherwise = let sqrtn1 = (floor $ sqrt $ fromIntegral n) + 1
                          smallestDivisor = filter (\x -> (mod n x) == 0) factors
                          in case smallestDivisor of
                                [] -> [n]
                                (x:xs) -> x : factorizeNonCached (n `div` x) factors

isPrime x = (length $ factorizeNonCached x guessPrimes) <= 1

primeTable = filter isPrime [2..nmax]

primeFactors n = zip factors powers
        where factors = map head groups
              powers = map length groups
              groups = (group $ factorizeNonCached n primeTable)


cartProd l1 l2 = [ x1 * x2 | x1 <- l1, x2 <- l2 ]

divisors n = foldl cartProd [1] divisorList
        where divisorList = [ [ i^j | j <- [0..k] ] | (i, k) <- pFactors ]
              pFactors = primeFactors n

sumDivisors n = (sum $ divisors n) - n

isAbundant n = (sumDivisors n) > n

abundantNumbers = filter isAbundant [12..nmax+5]

sum2AbundantNumbers = set
                where set = Set.fromList [ x + y | x <- abundantNumbers, y <- abundantNumbers, x + y <= nmax ]

complement = Set.difference naturals sum2AbundantNumbers
        where naturals = Set.fromList [1..nmax]

sumComplement = sum $ Set.toList complement
