module Splits
( splits
, kSplits
) where

import Data.Maybe (maybeToList)

-- calculate the ways to split n in a sum of k numbers

splits :: [Int] -> Int -> [[Int]]
splits elems n = let s = splits' elems n []
                 in map reverse s

splits' :: [Int] -> Int -> [Int] -> [[Int]]
splits' _ 0 acc = [acc]
splits' [] n _ | n/=0 = []
splits' (e:es) n acc = (splits' es (n-e) (e:acc)) ++ (splits' es n acc)

kSplits k elems n = filter (\ l -> n == (sum l) && (length l) == k) (splits elems n)
