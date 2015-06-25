maxReasonableNumber pow = head $ dropWhile largerThanLimit pairs
        where ints = [1..]
              maxSumPowDigits = map (\x -> x*9^pow) ints
              tenToTheNs = map (\x -> 10^x - 1) ints
              pairs = zip maxSumPowDigits tenToTheNs
              largerThanLimit = \(x,y) -> x>y

sumOfDigitsPow pow n
        | n<10 = n^pow
        | otherwise = (n `mod` 10)^pow + sumOfDigitsPow pow (n `div` 10)

fixedPoints pow = filter predicate interval
        where predicate = (\x -> x==sumOfDigitsPow pow x)
              interval = [2..maxN]
              maxN = snd $ maxReasonableNumber pow

sumFixedPoints pow = sum $ fixedPoints pow
