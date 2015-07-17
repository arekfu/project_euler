import Utils
import Data.Ratio
import Data.List

coeffsTimesTwo :: Integer -> (Integer, Integer)
coeffsTimesTwo k = (4-k, k-2)

polygonalNumber :: Integer -> Integer -> Integer
polygonalNumber k n = (a*n + b*n^2) `div` 2
        where ~(a,b) = coeffsTimesTwo k

polygonalNumbers :: Integer -> [Integer]
polygonalNumbers k = map (polygonalNumber k) [1..]

invPolygonal n k = floor $ snd $ solve2ndDegree (-2*k, fromIntegral a, fromIntegral b)
        where ~(a,b) = coeffsTimesTwo n

fourDigitPolygonal k = takeWhile (<=9999) $ dropWhile (<=999) $ polygonalNumbers k

limits n | low>9 = (low, hi)
         | otherwise = (1001,1000)
         where low = (n `mod` 100) * 100
               hi = low + 99

polygonalNumbersInInterval k (low, hi) = filter (\n -> n>999 && n<=9999) numbers
        where smallest = (invPolygonal k $ fromIntegral (low-1)) + 1
              largest = invPolygonal k $ fromIntegral hi
              numbers = map (polygonalNumber k) [smallest..largest]

--step :: Integer -> Integer -> [[Integer]]
--step k n = map (\x -> [x,n]) interval
--         where lims = limits n
--               interval = polygonalNumbersInInterval k lims

stepOnList :: Integer -> [Integer] -> [[Integer]]
stepOnList k list@(n:ns) = map (\x -> x:list) interval
         where lims = limits n
               interval = polygonalNumbersInInterval k lims

fourDigitOctagonals = map (\n->[n]) $ fourDigitPolygonal 8

testPermutation :: [Integer] -> [[Integer]]
testPermutation perm = (foldr (.) id $ map (concatMap . stepOnList) perm) fourDigitOctagonals

filteredTestPermutation :: [Integer] -> [[Integer]]
filteredTestPermutation perm = filter isCyclic $ testPermutation perm
        where isCyclic l = ((head l) `mod` 100) == ((last l) `div` 100)

candidates = concatMap filteredTestPermutation $ generatePermutations [3..7]

answer = map sum candidates

main = print answer
