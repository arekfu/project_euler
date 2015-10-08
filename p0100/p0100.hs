import Data.List.Ordered

main = print answer

answer = fst $ head $ dropWhile (\(a,b) -> b<10^12) solutions

sols = 3 : 105 : 3570 : (zipWith (\anm1 an -> 3 - anm1  + 34*an) (tail sols) (tail $ tail sols))

blueCircles = map toBlueCircles sols

solveTriangular n = let a = 1
                        b = 1
                        c = -2*n
                    in ((-b) + (round $ sqrt $ fromIntegral $ b^2 - 4*a*c)) `div` (2*a)

toBlueCircles n = 1 + solveTriangular n

toTotalCircles blue = 1 + (solveTriangular $ blue*(blue-1))

totalCircles = map toTotalCircles blueCircles

solutions = zip blueCircles totalCircles
