import Debug.Trace

permutationRank = 1000000-1

problemAlphabet = "0123456789"

ithPermutation :: Integer -> [Char] -> [Char]
ithPermutation rank alphabet
--        | trace (show index ++ " " ++ show factn1 ++ " ") False = undefined
        | rank > (fromIntegral n)*factn1 = error "rank must be <= (length alphabet)!"
        | n==1 = alphabet
        | otherwise = firstChar : ithPermutation (rank `mod` factn1) remainingAlphabet
        where n = length alphabet
              factn1 = factorial $ fromIntegral (n-1)
              index = fromIntegral $ rank `div` factn1
              (firstChar, remainingAlphabet) = splitStringAt alphabet index

factorial :: Integer -> Integer
factorial 0 = 1
factorial n | n>0 = n * factorial (n-1)

splitStringAt :: String -> Int -> (Char, String)
splitStringAt s i = (char, rest)
        where char = s !! i
              rest = (take i s) ++ (drop (i+1) s)
