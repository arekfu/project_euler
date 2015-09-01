import Data.Ratio

data Square =
    Go |
    A1 |
    CC1 |
    A2 |
    T1 |
    R1 |
    B1 |
    CH1 |
    B2 |
    B3 |
    Jail |
    C1 |
    U1 |
    C2 |
    C3 |
    R2 |
    D1 |
    CC2 |
    D2 |
    D3 |
    FP |
    E1 |
    CH2 |
    E2 |
    E3 |
    R3 |
    F1 |
    F2 |
    U2 |
    F3 |
    GoToJail |
    G1 |
    G2 |
    CC3 |
    G3 |
    R4 |
    CH3 |
    H1 |
    T2 |
    H2
    deriving (Show, Eq, Ord, Enum, Bounded)

next :: Square -> Square
next H2 = Go
next s = succ s

prev :: Square -> Square
prev Go = H2
prev s = pred s

advance :: (Integral a, Enum b, Bounded b) => a -> b -> b
advance n s = toEnum $ ((fromIntegral n) + (fromEnum s)) `mod` (1 + fromEnum (maxBound `asTypeOf` s))

newtype RollProbabilities

dieProbabilities :: Integral a => a -> [(a, Rational)]
dieProbabilities nSides = zip [1..nSides] (repeat (1 % (fromIntegral nSides)))

combineDieProbabilities :: 
combineDieProbabilities diceSides = undefined
