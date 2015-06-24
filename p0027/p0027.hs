import Primes
import qualified Data.Set as Set
import qualified Data.List as List

primeTable1000 = takeWhile (<1000) primeTable

primeSet = Set.fromList primeTable1000

poly :: Integer -> Integer -> Integer -> Integer
poly a b n = (n^2) + (a*n) + b

quadraticPrimes a b = primes
        where primes = takeWhile isPrime $ map (poly a b) [0..]

maxNPrimes a b = length primes
        where primes = quadraticPrimes a b

maxNPrimesWithB b = maximum $ nPrimesWithB b

nPrimesWithB b = zip3 nPrimes aVal (repeat b)
        where nPrimes = map (\a -> maxNPrimes a b) aVal
              aVal = (aValues b)

aValues b = filter (\a -> isPrime (a+b+1)) [minA..999]
        where minA = max (-999) ((-b)-1)

maxNPrimesOverRange = maximum $ map maxNPrimesWithB primeTable1000

