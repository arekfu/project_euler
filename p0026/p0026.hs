import Data.List (findIndex)

decimalExpansion :: Integer -> [(Integer,Integer)]
decimalExpansion n = decimalExpansionHelper n 1

decimalExpansionHelper :: Integer -> Integer -> [(Integer,Integer)]
decimalExpansionHelper n i = (d, r) : decimalExpansionHelper n (10*r)
        where d = i `div` n
              r = i `mod` n

findCycleLength :: (Eq a) => [a] -> Int
findCycleLength [] = 0
findCycleLength (x:xs) = case firstMatch of
        Nothing  -> findCycleLength xs
        (Just n) -> n + 1
        where firstMatch = findIndex (==x) xs

longestCycle = maximum $ zip cycleLengths range
        where cycleLengths = map findCycleLength decimalExpansions
              decimalExpansions = map ((take 1500) . decimalExpansion) range
              range = [3..1000]
