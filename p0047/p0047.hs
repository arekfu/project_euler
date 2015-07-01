import Primes
import Utils

lengthGreater3 :: [a] -> Bool
lengthGreater3 (a:b:c:d:xs) = True
lengthGreater3 _ = False

morethanFourFactors = lengthGreater3 . primeFactors

pairs = map (\n -> (n,morethanFourFactors n)) [2..]

firstFourTrue :: [(Integer, Bool)] -> Integer
firstFourTrue ((n1,b1):(n2,b2):(n3,b3):(n4,b4):xs)
        | b1 && b2 && b3 && b4 = n1
        | otherwise = firstFourTrue ((n2,b2):(n3,b3):(n4,b4):xs)

answer = firstFourTrue pairs

main = print answer

