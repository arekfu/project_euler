import Primes
import Utils
import Data.Array
import Data.List

nmax = 10000000

primePowers = [ (1,1) : [ (n, (p-1) * nOverP) |
                let kmax = floor $ (log (fromIntegral nmax))/(log (fromIntegral p)),
                k <- [1..kmax],
                let nOverP = p^(k-1), let n = nOverP * p,
                n < nmax ] | p <- takeWhile (<nmax) primes ]

--products = products' primePowers (tail primePowers)
--    where products' _ [] = []
--          products' ((x1,x2):xs) l@((y1,y2):ys) = (map (\(a,b) -> (x1*a, x2*b)) l) ++ (products' xs ys)

--allProducts :: [(Integer, Integer)]
--allProducts = allProducts' primePowers (tail primePowers)
--    where allProducts' xs [] = xs
--          allProducts' ((x1,x2):xs) yl =
--            (concatMap (\(a1, a2) -> (x1*a1, x2*a2)) yl) ++ (allProducts' xs (tail yl))

(*%) :: [(Integer, Integer)] -> [(Integer, Integer)] -> [(Integer, Integer)]
(*%) xs ys = [ (z1, z2) | (x1, x2) <- xs, (y1, y2) <- ys, let z1 = x1*y1, let z2 = x2*y2, z1<nmax ]
infixr 5 *%

--phi = listArray (1,nmax-1) $ 1 : [ phi' i | i <- [2..nmax-1] ]
--    where phi' i = let factors = getFactors $ runFactorization factorizer i
--                  in case factors of
--                    [(p,_)] -> p-1
--                    ps  -> foldl1' (*) $ map (phi !) ps

factorizer = makeFactorizer nmax

phi = zip [2..nmax] $ map (foldForPhi . (runFactorization factorizer)) [2..nmax]

permutationPhis = filter isPermutation phi
    where isPermutation (a,b) = null $ (numberToDigits a) \\ (numberToDigits b)

answer = minimumBy (\(a,b) (c,d) -> compare (a*d) (b*c)) permutationPhis

main = print answer
