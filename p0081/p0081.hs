import Data.Array
import Data.Foldable

main = do
        input <- readFile "p081_matrix.txt"
        print $ process input

process input = solve (matrixArray $ toMatrix input) ! (1,1)

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

replace array i j x = array // [ ((i,j),x) ]

update minArray i j = replace minArray i j $ (minArray ! (i,j)) + (min (minArray ! (i,j+1)) (minArray ! (i+1,j)))

solve array = foldl' (\arrx i -> foldl' (\arry j -> update arry i j) arrx [y1-1,y1-2..1]) minArray [x1-1,x1-2..1]
    where minArray = lastRow $ lastColumn array
          ((_,_),(x1,y1)) = bounds array

lastRow array = foldl' (\arr i -> replace arr i y1 $ sumRow arr i) array [x1-1,x1-2..1]
    where sumRow arr i = arr ! (i,y1) + arr ! (i+1,y1)
          ((_,_),(x1,y1)) = bounds array

lastColumn array = foldl' (\arr j -> replace arr x1 j $ sumCol arr j) array [y1-1,y1-2..1]
    where sumCol arr j = arr ! (x1,j) + arr ! (x1,j+1)
          ((_,_),(x1,y1)) = bounds array

