import Primes
import Data.Map (assocs)
import Data.Array.ST
import qualified Data.Array as A
import Control.Monad
import qualified Data.List as L
import Data.List (permutations)
import Data.List.Ordered (nub, sort)

f = makeFactorizer 500000

primeFactorize n = runFactorization f n

--solution n = 
--    where factors = getFactors $ factorize n

replicateFactors :: Factorization -> [[Integer]]
replicateFactors fact = L.nub $ permutations $ concatMap (\(p, k) -> replicate (fromIntegral k) (fromIntegral p)) asss
    where asss = assocs $ getFactors fact

allSplits :: [Integer] -> [[[Integer]]]
allSplits [f] = [[[f]]]
allSplits (f:fs) = let rest = allSplits fs
                   in (map ([f] :) rest) ++ (map (aggregate f) rest)
                   where aggregate x ((l:ls):lss) = (x:l:ls) : lss
--                         aggregate _ [] = []

allFactorizations :: Integer -> [[Integer]]
allFactorizations n = map (map product) splits
    where splits = concatMap allSplits $ replicateFactors fact
          fact = primeFactorize n

newtype SumAndLength = SL { getSumAndLength :: (Integer, Integer) } deriving (Eq, Ord)

instance Show SumAndLength where
    show (SL sl) = show sl

getSum :: SumAndLength -> Integer
getSum (SL (s, _)) = s

getLength :: SumAndLength -> Integer
getLength (SL (_, l)) = l

sums :: Integer -> [SumAndLength]
sums n = filter (\(SL (_, l)) -> l>1) all
    where all = map (\l -> SL (sum l, n - sum l + fromIntegral (length l))) $ allFactorizations n

solutionsArr :: Int -> A.Array Int Int
solutionsArr n = let n' = 2*n in runSTArray $ do
    sols <- newArray (2, n') maxBound
    forM_ [n',n'-1..2] $ \k -> do
        let ss = sums (fromIntegral k)
        forM_ ss $ \(SL (_, l)) -> do
            let l' = fromInteger l
            writeArray sols (fromInteger l) k
    return sols

solutions :: Int -> [Int]
solutions n = nub $ sort $ take (n-1) $ A.elems $ solutionsArr n

answer n = sum $ solutions n

main = print $ answer 12000
