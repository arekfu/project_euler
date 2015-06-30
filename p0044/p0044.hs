pent n = n * (3*n-1) `div` 2

pentn = take 10000 $ map pent [1..]

intPairsSum s = [ (i, j) | i <- [1..(s-1)], let j=s-i, j>i ]

intPairs = concat $ map intPairsSum [1..]

absPentn = [ (n1, n2, n2-n1) | (i,j) <- intPairs, let n1 = pent i, let n2 = pent j, isPentagonal (n2-n1), isPentagonal (n1+n2)]

main = print absPentn

isPerfectSquare :: Integer -> Bool
isPerfectSquare n = (round $ sqrt $ fromIntegral n)^2 == n

isPentagonal :: Integer -> Bool
isPentagonal n = isPerfectSquare discriminant && (1 + (round $ sqrt $ fromIntegral discriminant)) `mod` 6 == 0
        where discriminant = 1 + 24*n
