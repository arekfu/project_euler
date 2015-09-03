import Data.Ratio
import Data.List
import Data.Monoid
import Data.Function
import Data.Tuple

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

advance :: (Enum b, Bounded b) => Int -> b -> b
advance n s = toEnum $ (n + (fromEnum s)) `mod` (1 + fromEnum (maxBound `asTypeOf` s))

-- types and functions for rolling dice

type Probability = Double

newtype Prob a = P { getProb :: (a, Probability) } deriving (Eq, Ord, Show)

combine :: Monoid a =>  Prob a -> Prob a -> Prob a
combine (P (d1,p1)) (P (d2,p2)) = P (d1 `mappend` d2, p1*p2)

newtype PDF a = PDF { getPDF :: [Prob a] } deriving (Eq, Ord, Show)

convolute :: (Monoid a, Eq a, Ord a) => PDF a -> PDF a -> PDF a
convolute (PDF l1) (PDF l2) = l
    where l = reduce $ combine <$> l1 <*> l2

reduce :: (Eq a, Ord a) => [Prob a] -> PDF a
reduce ls = PDF reduced
    where grouped = groupBy ((==) `on` fst . getProb) $ sort ls
          reduced = map (sumTuples . map getProb) grouped
          sumTuples ts = P (x, sum $ map snd ts)
              where x = fst $ head ts

instance (Monoid a, Eq a, Ord a) => Monoid (PDF a) where
    mempty = PDF [P (mempty, 1)]
    mappend = convolute

instance Functor Prob where
    fmap f (P (a, p)) = P (f a, p)

instance Functor PDF where
    fmap f (PDF l) = PDF (map (fmap f) l)

type RollProbabilityList = PDF (Sum Int)

dieProbabilities :: Int -> RollProbabilityList
dieProbabilities nSides = PDF $ map P $ zip (map Sum [1..nSides]) (repeat (1 / (fromIntegral nSides)))

nDice :: [Int] -> RollProbabilityList
nDice [] = mempty
nDice (n:ns) = (dieProbabilities n) `mappend` (nDice ns)

-- types and functions for advancing on the board

type SquareProbabilityList = PDF Square

dispatch :: Prob Square -> SquareProbabilityList
dispatch (P (s, p)) = scale p $ dispatch' s

dispatch' :: Square -> SquareProbabilityList
dispatch' GoToJail = PDF [P (Jail, 1)]
dispatch' CC1 = PDF [P (CC1, 14/16), P (Go, 1/16), P (Jail, 1/16)]
dispatch' CC2 = PDF [P (CC2, 14/16), P (Go, 1/16), P (Jail, 1/16)]
dispatch' CC3 = PDF [P (CC3, 14/16), P (Go, 1/16), P (Jail, 1/16)]
dispatch' CH1 = PDF [P (CH1, 6/16), P (Go, 1/16), P (Jail, 1/16), P (C1, 1/16), P (E3, 1/16), P (H2, 1/16), P (R1, 1/16), P (R2, 2/16), P (U1, 1/16), P (T1, 1/16)]
dispatch' CH2 = PDF [P (CH2, 6/16), P (Go, 1/16), P (Jail, 1/16), P (C1, 1/16), P (E3, 1/16), P (H2, 1/16), P (R1, 1/16), P (R3, 2/16), P (U2, 1/16), P (D3, 1/16)]
dispatch' CH3 = reduce $ normal ++ (getPDF $ scale (1/16) $ dispatch' CC3)
    where normal = [P (CH3, 6/16), P (Go, 1/16), P (Jail, 1/16), P (C1, 1/16), P (E3, 1/16), P (H2, 1/16), P (R1, 3/16), P (U1, 1/16)]
dispatch' s = PDF [P (s, 1)]

normalization :: PDF a -> Probability
normalization (PDF l) = sum $ map (snd . getProb) l

move :: RollProbabilityList -> Square -> SquareProbabilityList
move rpl s = dispatched
    where rs = getPDF rpl
          advance' x = map (\(P (d,p)) -> P (advance (getSum d) x, p))
          dispatched = reduce $ concatMap (getPDF . dispatch) $ advance' s rs

scale :: Probability -> PDF a -> PDF a
scale p (PDF l) = PDF $ map (\(P (x, p')) -> P (x, p * p')) l

apply :: RollProbabilityList -> SquareProbabilityList -> SquareProbabilityList
apply rpl (PDF spl) = reduce $ concatMap (\(P (s, p)) -> getPDF $ scale p $ move rpl s) spl

uniformPDF = PDF $ map P $ zipWith (,) (enumFrom Go) (repeat (1/40))

pdf = (iterate (apply (nDice [4,4])) uniformPDF) !! 80

threeMost = reverse $ drop 36 $ sort $ map (swap . getProb) $ getPDF pdf

answer = map (fromEnum . snd) threeMost

main = print answer
