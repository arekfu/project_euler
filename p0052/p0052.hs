import qualified Data.Set as Set
import Data.List

multiples :: Int -> [Int]
multiples n = map (n*) [1..6]

digitMultiples :: Int -> Set.Set String
digitMultiples n = Set.fromList $ map (sort . show) $ multiples n

answer = take 1 solutions
        where solutions = filter (\(a,s) -> (length s)==1) $ zip [1..] $ map digitMultiples [1..]
