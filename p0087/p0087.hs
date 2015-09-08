import Primes
import Data.List.Ordered (sort, nub)

nmax = 50000000

somePrimes = takeWhile (<=limit) primes
    where limit = ceiling $ sqrt $ fromIntegral nmax

squares = prune $ map (^2) somePrimes
cubes = prune $ map (^3) somePrimes
fourths = prune $ map (^4) somePrimes

prune = filter (<nmax)

solutions = nub $ sort $ prune $ (+) <$> (prune $ (+) <$> fourths <*> cubes) <*> squares

answer = length solutions

main = do
        putStrLn $ "squares=" ++ (show $ length squares)
        putStrLn $ "cubes=" ++ (show $ length cubes)
        putStrLn $ "fourths=" ++ (show $ length fourths)
        print answer

