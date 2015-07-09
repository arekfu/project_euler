import Data.List
import Data.Array (listArray, (!))
import Primes
import Utils

maxTest = 15000

testPrimeTable = makePrimeTable maxTest

nT = (length testPrimeTable) - 1

testPrimeArray = listArray (0,nT) testPrimeTable

arraySize = 100000000
primeArray = makePrimeArray arraySize

primeTest n = if n<arraySize then primeArray ! n else isPrime n

data PrimeTuple a b = PrimeTuple { list :: [a], index :: b } deriving (Show, Eq, Ord)

emptyTuple = PrimeTuple [] (-1)

appendToPrimeTuple tuple = [ PrimeTuple newList i  | let j = (index tuple) + 1,
                i <- [j..nT],
                let p = testPrimeArray ! i,
                let oldList = list tuple,
                let newList = p:oldList,
                let cs = concatMap (\n -> [concatNumbers [p,n], concatNumbers [n,p]]) oldList,
                all primeTest cs
                ]

primeTuples = appendToPrimeTuple emptyTuple

pairs = sort $ concatMap appendToPrimeTuple primeTuples

triplets = sort $ concatMap appendToPrimeTuple pairs

quadruplets = sort $ concatMap appendToPrimeTuple triplets

quintuplets = sort $ concatMap appendToPrimeTuple quadruplets

main = do
        putStrLn $ "number of pairs: " ++ (show $ length pairs)
        putStrLn $ "number of triplets: " ++ (show $ length triplets)
        putStrLn $ "number of quadruplets: " ++ (show $ length quadruplets)
        putStrLn $ "number of quintuplets: " ++ (show $ length quintuplets)
        print $ take 10 quintuplets
        print $ map sum $ map list quintuplets
