module ContinuedFractions
( continuedFractionExpansionSqrt
, periodicContinuedFractionExpansionSqrt
, fixedPart
, periodicPart
, foldToRational
--, scanToRational
, toCFExpansion
, fromCFExpansion
, nthApproximant
, approximant
, approximants
) where

import Data.Ratio
import Utils

data CFStep = CFStep { term :: Integer, inSqrt :: Integer, onNum :: Integer, onDen :: Integer }
              deriving (Eq, Show, Ord)

nextStep (CFStep t a num den) = let sqrtA = sqrt (fromIntegral a)
                                    den' = (a - num^2) `div` den
                                    asFloat = (fromIntegral den) * (sqrtA - fromIntegral num) / (fromIntegral (a - num^2))
                                    t' = floor asFloat
                                    a' = a
                                    num' = (-num) - den' * t'
                                    in CFStep t' a' num' den'

newtype CFExpansion = CFExpansion [Integer] deriving (Show, Eq)

toCFExpansion :: [Integer] -> CFExpansion
toCFExpansion = CFExpansion

fromCFExpansion :: CFExpansion -> [Integer]
fromCFExpansion (CFExpansion l) = l

continuedFractionExpansionSqrt :: Integer -> CFExpansion
continuedFractionExpansionSqrt n = CFExpansion $ map term $ cfSteps n

cfSteps n = cfSteps' (CFStep intPart n (-intPart) 1)
        where intPart = floor $ sqrt $ fromIntegral n
              cfSteps' step@(CFStep t a num den) | den==0 = []
                                                 | otherwise = step : (cfSteps' $ nextStep step)

data PeriodicCFExpansion = PeriodicCFExpansion { fixedPart :: [Integer], periodicPart :: [Integer] }
                           deriving (Show, Eq)

periodicContinuedFractionExpansionSqrt :: Integer -> PeriodicCFExpansion
periodicContinuedFractionExpansionSqrt n = PeriodicCFExpansion fixed period
        where (fixedTerms, periodTerms) = splitAt periodStartIndex steps
              fixed = map term fixedTerms
              period = map term $ take periodLength periodTerms
              steps = cfSteps n
              (periodStartIndex, secondPeriodIndex) = firstRepeatingIndex steps
              periodLength = secondPeriodIndex - periodStartIndex

foldToRational :: CFExpansion -> Rational
foldToRational (CFExpansion []) = 0%1
foldToRational (CFExpansion [x]) = x % 1
foldToRational (CFExpansion (x:xs)) = x % 1 + recip (foldToRational $ CFExpansion xs)

--scanToRational :: CFExpansion -> [Rational]
--scanToRational e = scanToRational' e []
--        where scanToRational' (CFExpansion []) l = (0%1) : l
--              scanToRational' (CFExpansion [x]) l = (x % 1) : l
--              scanToRational' (CFExpansion (x:xs)) = (x % 1 + recip (scanToRational' (CFExpansion xs) l)) : l

nextApproximant :: Integer -> Rational -> Rational
nextApproximant term approx = recip $ (term%1) + approx

nthApproximant :: Int -> CFExpansion -> Rational
nthApproximant n e = (foldr nextApproximant (0%1) xs) + (x%1)
        where (x:xs) = take n $ fromCFExpansion e

approximant :: CFExpansion -> Rational
approximant e = (foldr nextApproximant (0%1) xs) + (x%1)
        where (x:xs) = fromCFExpansion e

approximants :: CFExpansion -> [Rational]
approximants e = map (+ (x%1)) $ scanr nextApproximant (0%1) xs
        where (x:xs) = fromCFExpansion e
