import Data.Function
import Data.List

nSolutions :: Integral a => (a, a) -> a
nSolutions (a,b) = a * (a+1) * b * (b+1) `div` 4

nmax = 500

target = 2000000

pairs = [ (i,j) | i <- [1..nmax], j <- [i..nmax] ]

allAreas = zip pairs $ map nSolutions pairs

byDiff = map (\(p, a) -> (p, abs (a - target))) allAreas

answer = sortBy (compare `on` snd) byDiff

main = do
        let ans = answer
        print $ take 50 ans
        print $ head ans
        putStrLn $ "area=" ++ show (uncurry (*) $ fst $ head ans)
