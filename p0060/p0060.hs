import Data.List
import qualified Data.Map as Map
import qualified Data.List.Ordered as Ordered
import qualified Data.Array as Array
import Primes
import Utils
import Debug.Trace

maxTest = 1000

testPrimeTable = takeWhile (<maxTest) primes

nT = (length testPrimeTable) - 1

data PrimalityTest = PrimalityTest { table :: Array.Array Integer Bool, test :: Integer -> Bool }

primalityTest = PrimalityTest arr (\n -> if (fromIntegral n)<=arrayMax then arr Array.! (fromIntegral n) else isPrime n)
        where arr = makePrimeArray arrayMax
              arrayMax = concatNumbers maxTest maxTest

primeTest n = test primalityTest $ n

data PrimeTuple = PrimeTuple { list :: [Integer], toBeConsidered :: [Integer] } deriving (Show, Eq, Ord)

--makePrimeTuple n = PrimeTuple [n] $ PrimeTuple (allowed [n])

emptyTuple = PrimeTuple [] $ tail testPrimeTable

--findNplet :: Int -> PrimeTuple -> Maybe [PrimeTuple]
--findNplet nmax tuple
--        | n >= nmax = Just [tuple]
--        | nleft <= 0 = Nothing
--        | otherwise = findQuintuplet nmax tuples
--        where n = length $ list tuple
--              nleft = length $ toBeConsidered tuple
--              tuples = concatMap 

makeChildren :: PrimeTuple -> [PrimeTuple]
makeChildren (PrimeTuple _ []) = []
makeChildren (PrimeTuple l (x:xs)) = (newTuple : (makeChildren newTuple)) ++ (makeChildren (PrimeTuple l xs))
        where newL = x:l
              newTuple = PrimeTuple newL (restOf x xs)

restOf :: Integer -> [Integer] -> [Integer]
restOf x xs = Ordered.isect xs $ primeMap Map.! x

answer = filter (\t -> (length $ list t) == 5) $ makeChildren emptyTuple

main = do
        let ans = answer
        print answer
        print $ head $ map (sum . list) ans

--appendToPrimeTuple tuple
--        | trace ("\n*** appendToPrimeTuple " ++ show tuple) False = undefined
--        | null $ toBeConsidered tuple = []
--        | otherwise = [ PrimeTuple newList remaining |
--                p <- toBeConsidered tuple,
--                let oldList = list tuple,
--                let newList = p:oldList,
--                let remaining = allowed newList
--                ]
--
isectMany :: (Ord a) => [[a]] -> [a]
isectMany [x] = x
isectMany (l:ls) = Ordered.isect l $ isectMany ls

allowed aList = isectMany $ map (primeMap Map.!) aList
--
--primeTuples = tail $ appendToPrimeTuple emptyTuple
--
primeMap = Map.fromList [ (p, qs) | p <- testPrimeTable,
                        let qs = [ q | q <- testPrimeTable,
                                   primeTest $ concatNumbers p q,
                                   primeTest $ concatNumbers q p
                                 ]
                        ]
--
--pairs = concatMap appendToPrimeTuple primeTuples
--
--triplets = concatMap appendToPrimeTuple pairs
--
--quadruplets = sort $ concatMap appendToPrimeTuple triplets
--
--quintuplets = sort $ concatMap appendToPrimeTuple quadruplets
--
--main = do
--        putStrLn $ "number of pairs: " ++ (show $ length pairs)
--        putStrLn $ "number of triplets: " ++ (show $ length triplets)
--        putStrLn $ "number of quadruplets: " ++ (show $ length quadruplets)
--        putStrLn $ "number of quintuplets: " ++ (show $ length quintuplets)
--        print $ take 10 quintuplets
--        print $ map sum $ map list quintuplets
