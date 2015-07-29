import Primes

nmax = 1000000

factorizer = makeFactorizer nmax

factorize = runFactorization factorizer

phi = map (phiFold . factorize) [2..nmax]

answer = sum phi
