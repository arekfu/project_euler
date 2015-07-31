import Primes
import Data.Array
import Debug.Trace

nMax = 100

arr = array ((0,0), (nMax, nMax)) $ [ ((n,d), countSplits' n d) | n <- [0..nMax], d <- [0..nMax] ]
countSplits' n largest | trace ("call: " ++ show n ++ " " ++ show largest) False = undefined
countSplits' n 0 | n>0 = 0
countSplits' 0 _ = 1
countSplits' n largest = sum $ map (\d -> trace ("arr ! " ++ show (n-d) ++ " " ++ show d) $ arr ! ((n-d), min d (n-d))) $ takeWhile (<=largest) primes
--countSplits' n largest = sum $ map (\d -> arr ! ((n-d), min d (n-d))) $ takeWhile (<=largest) primes

countSums n = (countSplits' n n) - (if isPrime n then 1 else 0)

answer = head $ dropWhile (\(_,b) -> b<=5000) $ zip [1..] $ map countSums [1..]
