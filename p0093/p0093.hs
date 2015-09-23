import Data.Ratio
import Data.List (permutations, maximumBy)
import Data.Maybe (catMaybes)
import Data.List.Ordered (sort, nub)
import Data.Function (on)
import Utils (zipMap)

data Expr = Value Rational
            | Add Expr Expr
            | Sub Expr Expr
            | Mul Expr Expr
            | Div Expr Expr
            deriving (Eq, Ord)

instance Show Expr where
    show (Value x) = show x
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (Mul e1 e2) = show e1 ++ " * " ++ show e2
    show (Div e1 e2) = show e1 ++ " / " ++ show e2

evaluate :: Expr -> Maybe Rational
evaluate (Value n) = return n
evaluate (Add e1 e2) = do
    x1 <- evaluate e1
    x2 <- evaluate e2
    return (x1+x2)
evaluate (Sub e1 e2) = do
    x1 <- evaluate e1
    x2 <- evaluate e2
    return (x1-x2)
evaluate (Mul e1 e2) = do
    x1 <- evaluate e1
    x2 <- evaluate e2
    return (x1*x2)
evaluate (Div e1 e2) = do
    x1 <- evaluate e1
    x2 <- evaluate e2
    if x2==0 then fail "division by zero" else return (x1/x2)

operations = [Add, Sub, Mul, Div]

allExprs :: [Int] -> [Expr]
allExprs l = concatMap allExprs' $ permutations $ map fromIntegral l
    where allExprs' [n] = [Value n]
          allExprs' (n:ns) = concat [ commute (op (Value n) rest) | op <- operations, rest <- allExprs' ns ]

commute :: Expr -> [Expr]
commute e@(Add e1 e2) = [e]
commute e@(Mul e1 e2) = [e]
commute e@(Sub e1 e2) = [e, Sub e2 e1]
commute e@(Div e1 e2) = [e, Div e2 e1]

values :: [Int] -> [Rational]
values rs = catMaybes $ map evaluate $ allExprs rs

intValues :: [Int] -> [Int]
intValues rs = map (fromIntegral . numerator) $ filter (\r -> r>0 && denominator r == 1) $ values rs

uniqueIntValues :: [Int] -> [Int]
uniqueIntValues rs = nub $ sort $ intValues rs

longestConsecutive :: [Int] -> Int
longestConsecutive l = length $ takeWhile id $ zipWith (==) l [1..]

solve :: [Int] -> Int
solve l = longestConsecutive $ uniqueIntValues l

allIntSeq :: [[Int]]
allIntSeq = [ [a, b, c, d] | d <- [4..9], c <- [3..d-1], b <- [2..c-1], a <- [1..b-1] ]

allSolutions = zipMap solve allIntSeq

answer = maximumBy (compare `on` snd) allSolutions
