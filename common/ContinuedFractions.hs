module ContinuedFractions
( continuedFractionExpansionSqrt
, periodicContinuedFractionExpansionSqrt
, fixedPart
, periodicPart
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

