import Data.List
import Primes
import Utils
import qualified Data.Set as Set

interval = [11..1000000]

candidates = filter (\n -> (n `mod` 3 /= 0) && (doesNotContain [2,4,6,8,0,5] n)) interval

doesNotContain :: [Integer] -> Integer -> Bool
doesNotContain l n = (length $ intersect l digits) == 0
        where digits = numberToDigits n

primeCandidates = Set.fromList $ filter isPrime candidates

circularPrimes = Set.filter isCircular primeCandidates
        where isCircular p = and (fmap (\x -> x `elem` primeCandidates) (circularPermutations p))

circularPermutations n = take nPerm (generateCircularPermutations n)
        where digits = numberToDigits n
              nPerm = (length digits) - 1
              generateCircularPermutations n = n' : generateCircularPermutations n'
                where n' = (n `div` 10) + (n `mod` 10)*10^nPerm

answer = length $ Set.union (Set.fromList [2,3,5,7]) circularPrimes
