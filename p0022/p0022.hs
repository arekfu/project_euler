import Data.Char (isAlpha, ord)
import Data.List (sort)
import Data.List.Split (splitOn)

main = do ls <- readFile "p022_names.txt"
          putStrLn $ show $ process ls
          return 0

process x = score x

score x = sum $ zipWith (*) [1..n] values
        where values = map stringValue namesList
              namesList = sort $ split $ sanitize x
              n = length namesList

sanitize x = filter (\y -> isAlpha y || y==',') x

split x = splitOn "," x

stringValue s = sum $ map charValue s

charValue c = (ord c) - (ord 'A') + 1
