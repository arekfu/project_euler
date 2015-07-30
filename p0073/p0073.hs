import Primes
import Utils
import Data.Ratio
import Debug.Trace
import qualified Data.Map as Map

factorizer = makeFactorizer 1000000
factorize = runFactorization factorizer

lowerBound f den = let denF = denominator f
                       numF = numerator f
                   in (((numF * den) - 1) `div` denF)

upperBound f den = let denF = denominator f
                       numF = numerator f
                   in (((numF * den) `div` denF) + 1)

nProperBetween f0 f1 den = nCoprimeBetween low upp den
    where facts = getFactors $ factorize den
          low = upperBound f0 den
          upp = lowerBound f1 den

nMultiplesBetween l u p = (dist + lRem - uRem) `div` p
    where lRem = (l-1) `mod` p
          uRem = u `mod` p
          dist = u - l + 1

nCoprimeBetween l u n = (u-l+1) - sumNMultiples l u n primes
    where primes = Map.keys $ getFactors $ factorize n

sumNMultiples l u n ps = sumNMultiples' l u n ps 1

sumNMultiples' _ _ _ facts k | (length facts) < k = 0
sumNMultiples' l u n facts k = (sum ns) - (sumNMultiples' l u n facts (k+1))
    where ns = map (nMultiplesBetween l u) products
          products = filter (<=n) $ map product $ samplesWithoutReplacement k facts

nProperBetweenWithMax f0 f1 denMax = sum $ map (nProperBetween f0 f1) [2..denMax]

answer = nProperBetweenWithMax (1%3) (1%2) 12000

main = do
         print answer
--         print $ nProperBetweenWithMax (0%1) (1%1) 1000000
