import Data.Ratio
import Utils
nextFraction x = 1 + 1 / (1 + x)

expansions = take 1000 $ iterate nextFraction (3%2)

answer = length $ filter (\x -> (numberOfDigits $ numerator x) > (numberOfDigits $ denominator x)) expansions

