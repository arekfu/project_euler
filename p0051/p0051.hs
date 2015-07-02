import Primes
import Utils
import Data.List
import Data.Ord

fiveDigitPrimes = takeWhile (<=99999) $ dropWhile (<=9999) primeTable

fiveCharStrings = map show fiveDigitPrimes

sixDigitPrimes = takeWhile (<=999999) $ dropWhile (<=99999) primeTable

sixCharStrings = map show sixDigitPrimes

replaceInString s indices char
        | indices==[] = s
        | otherwise = replaceInString s' (tail indices) char
        where i = head indices
              s' = (take i s) ++ (char : drop (i+1) s)

allDigitReplacements :: String -> [Int] -> [String]
allDigitReplacements s indices = map (replaceInString s indices) theDigits
        where theDigits = "0123456789"

allPrimeDigitReplacements s indices = filter (isPrimeWithTableUpToN 1000) $ map read $ filter (\s' -> head s' /= '0') $ allDigitReplacements s indices

possibleSubsequences s = filter (\l -> (length l)>1 && isGoodSub l) $ subsequences [0..(n-1)]
        where n = length s
              isGoodSub l = allEqual (map (s !!) l)

allEqual :: (Eq a) => [a] -> Bool
allEqual (x:y:xs) = x==y && allEqual (y:xs)
allEqual [x] = True

stringsWithMask stringList = [ (s,ms) | s <- stringList, let ms=possibleSubsequences s, (length ms)>0]

allPossibleReplacements strings = map (\(s,ms) -> map (allPrimeDigitReplacements s) ms) $ stringsWithMask strings

longestSequence strings = maximumBy (\a -> \b -> if ((length a) == (length b)) then (compare (head b) (head a)) else (compare (length a) (length b))) $ concat $ allPossibleReplacements strings

main = print $ longestSequence sixCharStrings
