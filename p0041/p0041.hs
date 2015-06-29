import Primes
import Utils

possibleLengths = [4, 7] -- because of divisibility by 3

genNumbers nDigits = map digitsToNumber $ map (\n -> ithPermutation n [1..nDigits]) [0..factn1]
        where factn1 = (factorial nDigits) - 1

panDigitalPrimes n = filter isPrime $ genNumbers n

answer = maximum $ panDigitalPrimes 7
