import Primes
import Utils

oddComposite = filter (\n -> not (isPrime n)) [3,5..]

violatesConjecture n = case decomposition of
        Nothing -> True
        Just x -> False
        where decomposition = findDecomposition n

findDecomposition :: Integer -> Maybe (Integer, Integer)
findDecomposition n = case decompositions of
                        [] -> Nothing
                        _ -> Just (head decompositions)
        where decompositions = [ (p, s) | p <- possiblePrimes, let s=(n-p) `div` 2, isPerfectSquare s ]
              possiblePrimes = takeWhile (<=n) (tail primeTable)

answer = head $ [ n | n <- oddComposite, violatesConjecture n ]

main = print answer
