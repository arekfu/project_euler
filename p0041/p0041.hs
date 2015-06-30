import Primes
import Utils

possibleLengths = [4, 7] -- because of divisibility by 3

panDigitalPrimes n = filter isPrime $ generateNumbers [1..n]

answer = maximum $ panDigitalPrimes 7
