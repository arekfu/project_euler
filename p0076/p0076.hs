import Data.Array
import Debug.Trace

nMax = 100

--countSplits n largest | trace ("call: " ++ show n ++ " " ++ show largest) False = undefined
countSplits n 0 | n>0 = 0
countSplits 0 _ = 1
countSplits _ 1 = 1
countSplits n largest = sum $ map (\d -> countSplits (n-d) $ min d (n-d)) [1..largest]

arr = array ((0,0), (nMax, nMax)) $ [ ((n,d), countSplits' n d) | n <- [0..nMax], d <- [0..nMax] ]
--countSplits' n largest | trace ("call: " ++ show n ++ " " ++ show largest) False = undefined
countSplits' n 0 | n>0 = 0
countSplits' 0 _ = 1
countSplits' _ 1 = 1
--countSplits' n largest = sum $ map (\d -> trace ("arr ! " ++ show (n-d) ++ " " ++ show d) $ arr ! ((n-d), min d (n-d))) [1..largest]
countSplits' n largest = sum $ map (\d -> arr ! ((n-d), min d (n-d))) [1..largest]

countSums n = (countSplits' n n) - 1

answer = countSums 100
