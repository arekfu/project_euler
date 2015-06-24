sumNumbersUpTo :: Integer -> Integer
sumNumbersUpTo n = n*(n+1) `div` 2

sumEvenNumbersUpTo :: Integer -> Integer
sumEvenNumbersUpTo n = k*(k+1)
        where k = n `div` 2

sumDiagonals :: Integer -> Integer
sumDiagonals 1 = 1
sumDiagonals n = sumDiagonals (n-2) + 4*((n-2)^2) + 10*step
        where k = (n+1) `div` 2
              step = n-1
