import Utils

isPalindrome n = isListPalindrome digits
        where digits = numberToDigits n

isListPalindrome l = l == reverse l

lychrelIterate n = reversed + n
        where reversed = digitsToNumber $ reverse $ numberToDigits n

producesPalindrome n = any isPalindrome $ take 50 $ tail $ iterate lychrelIterate n

answer = 10000 - (length $ filter id $ map producesPalindrome [1..10000])
