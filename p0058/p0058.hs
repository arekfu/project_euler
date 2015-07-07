import Data.Ratio
import Primes

cornerNumbers :: Integer -> [Integer]
cornerNumbers sideLength
        | sideLength `mod` 2 == 0 || sideLength<0 = error "Side length must be an odd integer"
        | sideLength==1 = [1]
        | otherwise = map (\n -> firstCorner-n*(sideLength-1)) [0..3]
        where firstCorner = sideLength^2
       
nDiagonalPrimes :: Integer -> Integer
nDiagonalPrimes 1 = 0
nDiagonalPrimes sideLength = fromIntegral (length $ filter (isPrimeWithTableUpToN 30000) $ tail $ cornerNumbers sideLength)

lengthDiagonals sideLength = 2*sideLength-1

nPrimes = map nDiagonalPrimes [7,9..]

cumulNPrimes = scanl (+) 5 nPrimes

pairs = zip [7,9..] (zip cumulNPrimes (map lengthDiagonals [7,9..]))

answer = head $ dropWhile (\(a,(b,c)) -> (10*b>=c)) pairs

main = print answer
