import Utils
import Data.Array

import Debug.Trace

nmax = 999999

arrMax = 6*(factorial 9)

factorialArr = array (0,9) $ zip [0..9] $ map factorial [0..9]

sumFactorialDigits n = sumFactDigArr ! n

sumFactDigArr = listArray (0,arrMax) $ (map factorial [0..9]) ++ [ sumFactorialDigits' i | i <- [10..arrMax] ]
    where sumFactorialDigits' j = (factorialArr ! (j `mod` 10)) + (sumFactDigArr ! (j `div` 10))

--loopLength n | trace ("call: " ++ show n) False = undefined
loopLength n | n==40585 || n==145 || n==1 || n==2 = 1
             | n==169 || n==363601 || n==1454 = 3
             | n==871 || n==45361 || n==872 || n==45362 = 2
             | otherwise = let next = sumFactDigArr ! n
                           in 1 + (nonRepLenArr ! next)

nonRepeatingLength n = (length nonRep) + (loopLength $ head rep)
    where (nonRep, rep) = span (\m -> (loopLength m)==0) $ iterate sumFactorialDigits n

nonRepLenArr = listArray (1,arrMax) [ loopLength i | i <- [1..arrMax] ]

lens = map (nonRepLenArr !) [1..nmax]
answer = length $ filter (==60) lens
