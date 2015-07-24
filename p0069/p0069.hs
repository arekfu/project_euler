import Primes

answer = last $ takeWhile (<1000000) $ scanl1 (*) primes
