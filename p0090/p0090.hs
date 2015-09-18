import Data.List
import qualified Data.List.Ordered as O
import Data.Char

newtype CubeDigit = CD { getDigit :: Char } deriving (Eq, Ord)

(==*) :: CubeDigit -> CubeDigit -> Bool
(CD '6') ==* (CD '9') = True
(CD '9') ==* (CD '6') = True
(CD a) ==* (CD b) = a == b
infix 4 ==*

allDigits = map (\i -> CD $ chr (i + ord '0')) [0..9]

compare' :: CubeDigit -> CubeDigit -> Ordering
compare' (CD '9') (CD b) = compare '6' b
compare' (CD a) (CD '9') = compare a '6'
compare' (CD a) (CD b) = compare a b

instance Show CubeDigit where
    show (CD a) = show a

squares = takeWhile (<100) $ map (^2) [1..]

squareDigits = map (map CD . pad 2 '0' . map (\c -> if c=='6' || c=='9' then 'S' else c) . show) squares

squareDigitsT = map toTuple squareDigits
    where toTuple [x,y] = (x,y)

pad :: Int -> a -> [a] -> [a]
pad n c s | l < n = replicate (n-l) c ++ pad (n-1) c s
          | otherwise = s
            where l = length s

newtype Cube = C { getCube :: [CubeDigit] } deriving (Eq, Ord)

instance Show Cube where
    show (C a) = show a

newtype CubeSplit = CS { getCubes :: (Cube, Cube) } deriving (Eq, Ord)

instance Show CubeSplit where
    show (CS a) = show a

generateSplits :: [(a,a)] -> [([a], [a])]
generateSplits ((x,y):xs) = concatMap (\(l, m) -> [(x:l, y:m), (y:l, x:m)]) $ generateSplits xs
generateSplits [] = [([],[])]

normalizeCube c = C $ nub $ sort c
normalizeSplit (c1, c2) = if c1'<c2' then CS (c1', c2') else CS (c2', c1')
    where c1' = normalizeCube c1
          c2' = normalizeCube c2

splits = map normalizeSplit s
    where s = generateSplits squareDigitsT

isValid :: CubeSplit -> Bool
isValid (CS (C c1, C c2)) = test c1 && test c2
    where test s = l<=6 && (length $ nub s) == l
            where l = length s

validSplits = nub $ filter isValid splits

replaceSInC (C cube) = nub $ map (C . sort . getCube) replaced
    where replaced = if (CD 'S') `elem` cube
                     then (replaceWith '6' cube) ++ (replaceWith '9' cube)
                     else [C cube]
          replaceWith char c = let new = replaceFirst char c
                               in replaceSInC (C new)

replaceFirst char c = case (break (==(CD 'S')) c) of
                        (_, []) -> c
                        (a, (x:xs)) -> a ++ ((CD char) : xs)

replaceSInCS (CS (c1, c2)) = [ CS (c1', c2') | c1' <- replaceSInC c1, c2' <- replaceSInC c2 ]

completeC (C c) = map (C . sort) $ filter (\l -> length l == 6) $ map nub $ completeC' c
    where completeC' x | length x == 6 = [x]
                       | otherwise = concatMap (\l -> completeC' l) $ map (:x) allDigits

completeCS (CS (c1, c2)) = [ CS (c1', c2') | c1' <- completeC c1, c2' <- completeC c2 ]

fullSplits = O.nub $ sort $ map (\ (CS (c1, c2)) -> if c1<c2 then CS (c1,c2) else CS (c2,c1)) $ (validSplits >>= replaceSInCS >>= completeCS)

answer = length fullSplits

