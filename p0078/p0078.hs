import Data.Array
import Debug.Trace

nMax = 1000000

-- use Euler's recurrence relation:
-- p(k) = p(k-1) + p(k-2) + p(k-5) + p(k-7) + ...
-- where g_k = 1, 2, 5, 7, ... are the generalized pentagonal numbers:
--g_k = k(3k-1)/2

arr = listArray (0, nMax) $ 1: [ sumOverPent n | n <- [1..nMax] ]

alternate [] ys = ys
alternate (x:xs) ys = x : alternate ys xs

ints = [1..]
negInts = [-1,-2..]

sumOverPent :: Integer -> Integer
sumOverPent 0 = 1
sumOverPent 1 = 1
sumOverPent n = sum $ map (\(i,p) -> (-1)^(i+1) * (arr!(n-p))) indices
    where pents = takeWhile (<=n) $ map generalizedPentagonal $ alternate ints negInts
          indices = zip (alternate ints ints) pents

generalizedPentagonal k = (k*(3*k-1)) `div` 2

----countSplits n largest | trace ("call: " ++ show n ++ " " ++ show largest) False = undefined
--countSplits n 0 | n>0 = 0
--countSplits 0 _ = 1
--countSplits _ 1 = 1
----countSplits n largest = sum $ map (\d -> trace ("arr ! " ++ show (n-d) ++ " " ++ show d) $ arr ! ((n-d), min d (n-d))) [1..largest]
--countSplits n largest
--    | n < largest = countSplits n n
--    | otherwise = sum $ map (\d -> arr ! ((n-d), min d (n-d))) [1..largest]

firstDivisible k = head $ dropWhile (\(_,b) -> b`mod`k /= 0) $ zip [1..] $ map sumOverPent [1..]
--firstDivisible k = head $ dropWhile (\(_,b) -> b`mod`k /= 0) $ zip [1..] $ map (\n -> countSplits n n) [1..]

--summationProperty n d = countSplits n d == sum (map (\k -> countSplits (n-k) k) [1..d])

answer = firstDivisible 1000000

main = print answer

{-
 -
 - o  o  o  o  o  o  o  o  x
 -
 - o  o  o  o  o  o  o  x  x
 -
 - o  o  o  o  o  o  x  x  x
 -
 - o  o  o  o  o  x  x  x  x
 -
 - o  o  o  o  x  x  x  x  x
 -
 - o  o  o  x  x  x  x  x  x
 -
 - o  o  3  4  5  x  x  x  x
 -
 - o  2  2  3  3  4  x  x  x
 -
 - 1  1  1  1  1  1  1  1  1
 ---------------------------
 - 1  2  3  4  5  6  7  8  9
 -}
