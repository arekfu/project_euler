type Roman = String

rm = 'M'
rd = 'D'
rc = 'C'
rl = 'L'
rx = 'X'
rv = 'V'
ri = 'I'

value :: Char -> Int
value 'I' = 1
value 'V' = 5
value 'X' = 10
value 'L' = 50
value 'C' = 100
value 'D' = 500
value 'M' = 1000
value _ = error "wrong character"

romanToInt :: Roman -> Int
romanToInt s = romanToInt' (reverse s) 0
    where romanToInt' (x:xs) last =
            let v = value x
            in (if v<last then (-v) else v) + romanToInt' xs v
          romanToInt' [] _ = 0

intToRoman :: Int -> Roman
intToRoman n = let thousands = n `div` 1000
                   n' = n - thousands * 1000
                   hundreds = n' `div` 100
                   n'' = n' - hundreds * 100
                   tens = n'' `div` 10
                   units = n `mod` 10
               in replicate thousands rm ++ romanUnit rc rd rm hundreds ++ romanUnit rx rl rc tens ++ romanUnit ri rv rx units

romanUnit :: Char -> Char -> Char -> Int -> Roman
romanUnit _ _ _ 0 = ""
romanUnit one five ten 4 = [one, five]
romanUnit one five ten 9 = [one, ten]
romanUnit one five ten n | n>4 = five : romanUnit one five ten (n-5)
                         | otherwise = replicate n one

canonical :: Roman -> Roman
canonical = intToRoman . romanToInt

solve :: [Roman] -> Int
solve = sum . map (\r -> length r - length (canonical r))

main = do
        content <- readFile "p089_roman.txt"
        let ls = lines content
        let sol = solve ls
        print sol
