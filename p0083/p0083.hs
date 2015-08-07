import Data.Array
import Data.Maybe (catMaybes)
import qualified Data.Heap as H
import qualified Data.Set as S

import Debug.Trace
--trace _ b = b

main = do
        input <- readFile "p083_matrix.txt"
        print $ process input

process input = lookup (Point (1,1)) $ solve $ matrixArray $ toMatrix input

type Score = Int

toMatrix ::  String -> [[Score]]
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

-- boilerplate ends here

newtype Point = Point (Int,Int) deriving (Show, Eq, Ord)

arr `at` (Point p) = arr ! p

solve arr = bfs (initialHeap arr) S.empty arr []

initialHeap :: (Ord a) => Array (Int,Int) a -> H.MinPrioHeap a Point
initialHeap arr = H.singleton (arr `at` p, p)
    where p = Point (x1,y1)
          ((_,_),(x1,y1)) = bounds arr

data Direction = DUp | DDown | DRight | DLeft deriving (Show, Eq, Ord, Ix)

move :: Array (Int, Int) Int -> Point -> Direction -> Maybe Point
move arr (Point (i,j)) DUp | i>x0 = Just (Point (i-1, j))
                       | otherwise = Nothing
    where ((x0,y0),(x1,y1)) = bounds arr
move arr (Point (i,j)) DDown | i<x1 = Just (Point (i+1, j))
                         | otherwise = Nothing
    where ((x0,y0),(x1,y1)) = bounds arr
move arr (Point (i,j)) DLeft | j>y0 = Just (Point (i, j-1))
                         | otherwise = Nothing
    where ((x0,y0),(x1,y1)) = bounds arr
move arr (Point (i,j)) DRight | j<y1 = Just (Point (i, j+1))
                          | otherwise = Nothing
    where ((x0,y0),(x1,y1)) = bounds arr

reachable :: Point -> Array (Int, Int) Int -> S.Set Point -> [Point]
reachable point arr visited = filter (\p -> not $ S.member p visited) (catMaybes points)
    where points = map (move arr point) [DUp, DDown, DRight, DLeft]

bfs ::  H.MinPrioHeap Int Point -> S.Set Point -> Array (Int, Int) Int -> [(Point,Int)] -> [(Point, Int)]
bfs heap _ _ values | H.null heap = values
--bfs heap visited arr values | trace ("---------------\n" ++ show values ++ "\n" ++ show visited) False = undefined
bfs heap visited arr values = bfs heap' visited'' arr values'
    where ((top:_), restHeap) = H.splitAt 1 heap
          (value, point) = top
          points = reachable point arr visited
          visited' = S.insert point visited
          visited'' = foldr S.insert visited' points
          pvals = map (\p -> (arr `at` p + value, p)) points
          heap' = foldr H.insert restHeap pvals
          newValue = (point, value)
          values' = newValue : values
