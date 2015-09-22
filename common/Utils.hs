module Utils
( cartProd
, factorial
, digitsToNumber
, numberToDigits
, numberToDigitsBackwards
, numberOfDigits
, allDifferentDigits
, ithPermutation
, generateDigitPermutations
, ithSampleWithoutReplacement
, unorderedSamplesWithoutReplacement
, generateNumbers
, isPentagonal
, isHexagonal
, isPerfectSquare
, concatNumbers
, concatListOfNumbers
, solve2ndDegree
, isOdd
, isEven
, isKthPower
, digitalSum
, firstRepeatingIndex
, pairElementsWith
, samplesWithoutReplacement
, zipMap
, generatePicks
)
where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (permutations, tails, subsequences, foldl', unfoldr)
import Data.Char (ord)
import Debug.Trace

cartProd l1 l2 = [ x1 * x2 | x1 <- l1, x2 <- l2 ]

factorial :: (Integral a) => a -> a
factorial n = product [1..n]

digitsToNumber :: (Num a) => [a] -> a
digitsToNumber digs = foldl' (\x y -> x*10+y) 0 digs

--numberToDigitsBackwards :: (Integral a) => a -> [a]
--numberToDigitsBackwards n
--    | n<10 = [n]
--    | otherwise = d : numberToDigitsBackwards (n `div` 10)
--    where d = n `mod` 10

numberToDigits :: (Integral a, Show a) => a -> [a]
numberToDigits n = map (\c -> fromIntegral ((ord c) - 48)) $ show n
-- ord '0' = 48

numberToDigitsBackwards n = unfoldr (\k -> if k>0 then (Just (k `mod` 10, k `div` 10)) else Nothing) n

numberOfDigits :: (Integral a, Show a) => a -> a
numberOfDigits n = fromIntegral $ length $ numberToDigits n

allDifferentDigits n = nDigits==nDifferentDigits
        where digits = numberToDigits n
              nDigits = length digits
              nDifferentDigits = length $ Set.fromList $ numberToDigits n

ithPermutation :: (Integral a) => a -> [b] -> [b]
ithPermutation rank alphabet
--        | trace (show index ++ " " ++ show factn1 ++ " ") False = undefined
        | rank > (fromIntegral n)*factn1 = error "rank must be <= (length alphabet)!"
        | n==1 = alphabet
        | otherwise = firstChar : ithPermutation (rank `mod` factn1) remainingAlphabet
        where n = length alphabet
              factn1 = factorial $ fromIntegral (n-1)
              index = fromIntegral $ rank `div` factn1
              (firstChar, remainingAlphabet) = extractListElementAt alphabet index

extractListElementAt :: (Integral b) => [a] -> b -> (a, [a])
extractListElementAt s i = (char, rest)
        where char = s !! iI
              rest = (take iI s) ++ (drop (iI+1) s)
              iI = fromIntegral i

generateNumbers digits = map digitsToNumber $ permutations digits

ithSampleWithoutReplacement :: (Integral a) => a -> a -> [b] -> [b]
ithSampleWithoutReplacement rank k alphabet
--        | trace ("+++" ++ show rank ++ " " ++ show nSamples ++ " " ++ show nSamples1 ++ "+++") False = undefined
        | rank>=nSamples = error "rank must be <= n! / (n-k)!"
        | k>n = error "sample size must be <= (length alphabet)!"
        | k==0 = []
        | otherwise = firstChar : ithSampleWithoutReplacement (rank `mod` nSamples1) (k-1) remainingAlphabet
        where n = fromIntegral $ length alphabet
              nSamples = nSamplesWithoutReplacement n k
              nSamples1 = nSamplesWithoutReplacement (n-1) (k-1)
              index = fromIntegral $ rank `div` nSamples1
              (firstChar, remainingAlphabet) = extractListElementAt alphabet index

unorderedSamplesWithoutReplacement :: (Integral a) => a -> [b] -> [[b]]
unorderedSamplesWithoutReplacement k l = concatMap permutations $ samplesWithoutReplacement k l

nSamplesWithoutReplacement :: (Integral a) => a -> a -> a
nSamplesWithoutReplacement n k = product [(n-k+1)..n]

isPerfectSquare :: (Integral a) => a -> Bool
--isPerfectSquare n = (round $ sqrt $ fromIntegral n)^2 == n
isPerfectSquare n = (head $ dropWhile (\k -> k^2>n) (iterate (next n) n))^2 == n
    where next n a = (a + (n `div` a)) ` div` 2

isPentagonal :: (Integral a) => a -> Bool
isPentagonal n = isPerfectSquare discriminant && (1 + (round $ sqrt $ fromIntegral discriminant)) `mod` 6 == 0
        where discriminant = 1 + 24*n

isHexagonal :: (Integral a) => a -> Bool
isHexagonal n = isPerfectSquare discriminant && (1 + (round $ sqrt $ fromIntegral discriminant)) `mod` 4 == 0
        where discriminant = 1 + 8*n

generateDigitPermutations :: (Integral a, Show a) => a -> [a]
generateDigitPermutations n = map digitsToNumber $ permutations $ numberToDigits $ fromIntegral n

concatListOfNumbers :: (Integral a) => [a] -> a
concatListOfNumbers [] = 0
concatListOfNumbers (n:ns) = n + factor*(concatListOfNumbers ns)
        where factor = head $ dropWhile (<=n) [ 10^k | k <- [0..]]

concatNumbers :: (Integral a) => a -> a -> a
concatNumbers m n = n + factor*m
        where factor = head $ dropWhile (<=n) [ 10^k | k <- [0..]]

solve2ndDegree :: (Floating a) => (a, a, a) -> (a, a)
solve2ndDegree (c,b,a) = ((-d)-sqrtDiscrOver2a, (-d)+sqrtDiscrOver2a)
        where d = b / (2.0 * a)
              discr = b**2 - 4.0 * a * c
              sqrtDiscrOver2a = (sqrt discr) / (2.0 * a)

isOdd n = n `mod` 2 /= 0
isEven n = n `mod` 2 == 0

isKthPower :: (Integral a, Integral b) => a -> b -> Bool
isKthPower k n = (round kthRoot)^k == n
        where rk = fromIntegral k
              rn = fromIntegral n
              kthRoot = (rn**((1.0::Double)/rk)) :: Double

digitalSum n | n<10 = n
             | otherwise = digitalSum $ sum $ numberToDigits n

firstRepeatingIndex :: (Ord a, Integral b) => [a] -> (b, b)
firstRepeatingIndex list = firstRepeatingIndex' list 0 Map.empty
        where firstRepeatingIndex' [] index _ = (index, index)
              firstRepeatingIndex' (x:xs) index m | x `Map.member` m = (m Map.! x, index)
                                                  | otherwise = firstRepeatingIndex' xs (index+1) (Map.insert x index m)

pairElementsWith :: (a -> a -> b) -> [a] -> [b]
pairElementsWith f l = concatMap (\l -> zipWith f (repeat $ head l) (tail l)) suffs
    where suffs = init $ tails l

samplesWithoutReplacement :: (Integral a) => a -> [b] -> [[b]]
samplesWithoutReplacement n l = filter (\l -> (length l)==(fromIntegral n)) $ subsequences l

zipMap :: (a -> b) -> [a] -> [(a, b)]
zipMap f l = zip l $ map f l

-- pick one element from each list inside the first list in all possible ways
generatePicks :: [[a]] -> [[a]]
generatePicks (x:xs) = concatMap (\f -> map f $ generatePicks xs) $ map (:) x
generatePicks [] = [[]]
