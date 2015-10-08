generator n = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10

generated :: [Int]
generated = map generator [1..]

cubes :: [Int]
cubes = map (^3) [1..]

derive s = zipWith (-) (tail s) s

regress [] = []
regress s = let len = length s
                pow = len - 1
                coeff = (head $ (iterate derive s) !! pow) `div` fact pow
                highest = map (\n -> coeff * n^pow) [1..]
                residue = take pow $ zipWith (-) s highest
            in coeff : regress residue

fact n = product [1..n]

op s = let len = length s
           order = len - 1
           coeffs = regress s
       in poly coeffs

--poly :: Num a => [a] -> [a]
poly coeffs = let len = length coeffs
                  order = len - 1
                  value n = sum $ zipWith (*) coeffs (map (n^) [order, order-1..0])
              in map value [1..]

fit n s = let s' = take n s
          in head $ dropWhile (uncurry (==)) $ zip (op s') s

sumFITsUpTo n s = sum (map (\k -> fst $ fit k s) [1..n])

answer = sumFITsUpTo 10 generated

main = print answer
