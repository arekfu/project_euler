britishDenominations = [ 1, 2, 5, 10, 20, 50, 100, 200 ]

countSplits :: (Integral a) => a -> [a] -> a
countSplits n dens = countSplits' n remaining
    where remaining = takeWhile (<=n) dens

countSplits' n [] | n>0 = 0
countSplits' 0 _ = 1
countSplits' _ [x] = 1
countSplits' n rem = sum $ map (\d -> countSplits (n-d) $ filter (<=d) rem) rem

answer = countSplits 200 britishDenominations
