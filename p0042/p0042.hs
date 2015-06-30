import Data.Char

main= do
        text <- readFile "p042_words.txt"
        let ans = countTriangularWords text
        print ans

countTriangularWords text = countTriangular $ words $ replacePunctuation text

replacePunctuation = map (\c -> if (isAlpha c) then c else ' ')

countTriangular l = length $ filter isTriangular l

triangularNumbers :: [Integer]
triangularNumbers = map (\n -> (n*(n+1)) `div` 2) [1..100]

isTriangular :: String -> Bool
isTriangular s = (sum $ map charScore s) `elem` triangularNumbers

charScore :: Char -> Integer
charScore c = fromIntegral $ ord c - ord 'A' + 1
