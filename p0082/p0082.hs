import Data.Array
import Data.Foldable
import Debug.Trace

main = do
        input <- readFile "p082_matrix.txt"
        print $ process input

--process input = minimum $ map (\i -> (solve inputArr) ! (Point (i,y0), DRight)) [x0..x1]
process input = minimum $ map (\i -> solve inputArr ! (Point (i,y0),DRight)) [x0..x1]
    where inputArr = matrixArray $ toMatrix input
          ((x0,y0),(x1,_)) = bounds inputArr

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

-- the boilerplate ends here
-- the real solution algorithm starts here

data Direction = DUp | DDown | DRight deriving (Show, Eq, Ord, Ix)

newtype Point = Point (Int, Int) deriving (Show, Eq, Ord, Ix)

move :: Point -> Direction -> (Point, Direction)
move (Point (i,j)) DUp = (Point (i-1, j), DUp)
move (Point (i,j)) DDown = (Point (i+1, j), DDown)
move (Point (i,j)) DRight = (Point (i, j+1), DRight)

right p = move p DRight
up p = move p DUp
down p = move p DDown

solve arr = minArray
    where minArray = array ((Point (x0,y0), DUp), (Point(x1,y1), DRight)) [ ((p, d), solve' p d) | p <- range (Point (x0,y0), Point (x1,y1)), d <- [DUp,DDown,DRight] ]
          ((x0,y0),(x1,y1)) = bounds arr
--          solve' p d | trace (show p ++ " " ++ show d ++ " " ++ show x1) False = undefined
          solve' p@(Point (i,j)) DUp | i==x0 = arr ! (i,j) + (minArray ! (right p))
          solve' p@(Point (i,j)) DUp | otherwise = arr ! (i,j) + (min (minArray ! (right p)) (minArray ! (up p)))
          solve' p@(Point (i,j)) DDown | i==x1 = arr ! (i,j) + (minArray ! (right p))
          solve' p@(Point (i,j)) DDown | otherwise = arr ! (i,j) + (min (minArray ! (right p)) (minArray ! (down p)))
          solve' p@(Point (i,j)) DRight | j==y1 = arr ! (i,j)
          solve' p@(Point (i,j)) DRight | i==x0 = arr ! (i,j) + (min (minArray ! (right p)) (minArray ! (down p)))
          solve' p@(Point (i,j)) DRight | i==x1 = arr ! (i,j) + (min (minArray ! (right p)) (minArray ! (up p)))
          solve' p@(Point (i,j)) DRight | otherwise = arr ! (i,j) + (minimum [minArray ! (right p), minArray ! (down p), minArray ! (up p)])
