import Data.Array
import Data.Foldable

main = do
        input <- readFile "p081_matrix.txt"
        print $ process input

process input = solve $ matrixArray $ toMatrix input

toMatrix ::  String -> [[Int]]
toMatrix input = map ((map read) . (split ',')) $ lines input

split :: (Eq a) => a -> [a] -> [[a]]
split _ [] = [[]]
split c s = case second of
                []     -> [first]
                (x:xs) -> first : split c xs
    where (first, second) = break (==c) s

matrixSize matrix = let y = length matrix
                        x = length $ head matrix
                    in (x,y)

matrixArray matrix = listArray ((1,1),(x,y)) $ concat matrix
    where (x,y) = matrixSize matrix

solve arr = minArray ! (1,1)
    where minArray = array ((x0,y0),(x1,y1)) [ ((i,j), solve' i j) | i <- [x0..x1], j <- [y0..y1] ]
          ((x0,y0),(x1,y1)) = bounds arr
          solve' i j | i==x1 = sum [ arr ! (x1,j') | j' <- [j..y1] ]
                     | j==y1 = sum [ arr ! (i',y1) | i' <- [i..x1] ]
                     | otherwise = arr ! (i,j) + (min (minArray ! (i+1,j)) (minArray ! (i,j+1)))
