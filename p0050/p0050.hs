import Primes

partialSums :: (Num a) => Integer -> [a] -> [a]
partialSums rank list@(x:xs)
        | (length list) < fromIntegral iRank = []
        | otherwise = (sum $ take iRank list) : (partialSums rank xs)
        where iRank = fromIntegral rank

revPrimeTable = reverse $ take 7000 primeTable

bestPrimeSum rank = take 1 $ filter (\n -> isPrime n && n<1000000) $ partialSums rank revPrimeTable

scan = map bestPrimeSum [543..571]
