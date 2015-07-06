comb n r = (product [(k+1)..n]) `div` (product [2..(n-k)])
        where k = max r (n-r)

nCombsNElementsOver x n = max result 0
        where result = (n+1) - 2*(length $ takeWhile (<=x) $ map (comb n) [0..n2])
              n2 = n `div` 2
              parity = (n+1) `mod` 2

answer = sum $ map (nCombsNElementsOver 1000000) [1..100]
