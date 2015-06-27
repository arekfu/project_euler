import Data.List
import Primes
import Utils

truncateRight :: Integer -> [Integer]
truncateRight 0 = []
truncateRight n | n>0 = n : truncateRight (n `div` 10)

truncateLeft :: Integer -> [Integer]
truncateLeft n
        | n>9 = n : truncateLeft ((read $ tail $ show n) :: Integer)
        | otherwise = [n]

allTruncRightPrimes n = and $ map isPrime $ truncateRight n
allTruncLeftPrimes n = and $ map isPrime $ truncateLeft n
allTruncPrimes n = (allTruncRightPrimes n) && (allTruncLeftPrimes n)

candidates = filter (\n -> n>7 && doesNotContain [2,4,6,8,0,5] n) primeTable

doesNotContain :: [Integer] -> Integer -> Bool
doesNotContain l n = (length $ intersect l digits) == 0
        where digits = numberToDigits n

validPrimes = drop 4 $ take 15 $ filter allTruncPrimes primeTable

answer = sum validPrimes
