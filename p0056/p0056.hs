import Utils
sumDigits n = sum $ numberToDigits n

answer = maximum [ sumDigits (a^b) | a <- [1..100], b <- [1..100] ]
