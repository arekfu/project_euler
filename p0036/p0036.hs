import Utils

isPalindrome n = isListPalindrome digits
        where digits = numberToDigits n

isListPalindrome l = l == reverse l

digitsToBase b n = reverse $ toBaseHelper b n []
        where toBaseHelper b n l
                | n==0 = l
                | otherwise = (n `mod` b) : toBaseHelper b (n `div` b) l

palindromes = filter (\n -> (isPalindrome n) && (isListPalindrome $ digitsToBase 2 n)) [1..1000000]
