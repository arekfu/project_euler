import Data.Array
import Debug.Trace

main = do ls <- readFile "p067_triangle.txt"
          putStrLn $ show $ process ls
          return 0

process x = minCost grid
            where grid = listArray ((1, 1), (n, n)) $ concat listGrid
                  n = length listGrid
                  listGrid = squarify $ parseGrid x

parseGrid :: String -> [[Integer]]
parseGrid x = map (map read) (map words $ lines x)

padList :: a -> Int -> [a] -> [a]
padList elem n list = list ++ replicate (n - len) elem
                      where len = length list

squarify :: [[Integer]] -> [[Integer]]
squarify x = map (padList (-9999) maxLen) x
           where maxLen = maximum $ map length x

minCost :: Array (Int,Int) Integer -> Integer
minCost grid = arr ! (1,1)
            where arr = listArray gridBounds [ minCost' grid x y | x <- [x0..x1], y <- [y0..y1] ]
                  gridBounds@((x0,y0), (x1,y1)) = bounds grid
                  minCost' grid x y
                        | trace ("minCost' " ++ show x ++ " " ++ show y) False = undefined
                        | x==x1 = grid ! (x,y)
                        | otherwise = grid ! (x,y) + max (arr ! (x+1,y)) (arr ! (x+1,y+1))

--minCost x | trace ("minCost " ++ show x) False = undefined
--minCost' :: Array Int Integer -> Int -> Int -> Integer
--minCost' [[x]] = x
--minCost' (x:xs) = head x + max (minCost leftTriangle) (minCost rightTriangle)
--                 where rightTriangle = map tail xs
--                       leftTriangle = map (take (len - 1)) xs
--                       len = length x
