import Data.List (findIndices, (\\))
import Data.Matrix
import qualified Data.Vector as V
import Data.Char (ord)
import Data.List.Ordered (minus, sort, sortOn, nub)
import Utils (zipMap)
import qualified Debug.Trace as D
import qualified Data.Set as S
import Control.Monad.State

type Sudoku = Matrix Int

main = do
    file <- readFile "p096_sudoku.txt"
    let ls = lines file
    let gs = grids $ gridsAs9Strings ls
    let solutions = map solveOne gs
    let checked = zipMap checkSolution solutions
    forM_ (zip checked [1..]) $ \((s, ok), i) -> do
        unless ok $ putStrLn "unsolved:"
        putStrLn $ (show i) ++ ":"
        print s
    print $ sum $ map getThreeDigitNumber solutions

first = do
    file <- readFile "p096_sudoku.txt"
    let ls = lines file
    let gs = grids $ gridsAs9Strings ls
    let solution = solveOne $ gs !! 9
    let ok = checkSolution solution
    unless ok $ putStrLn "unsolved:"
    print solution

getThreeDigitNumber :: Sudoku -> Int
getThreeDigitNumber s = 100*(getElem 1 1 s) + 10*(getElem 1 2 s) + (getElem 1 3 s)

gridsAs9Strings ls = map tail $ chunks 10 ls

stringsToInts :: [String] -> [[Int]]
stringsToInts grid = map (map chrToInt) grid
    where chrToInt c = ord c - ord '0'

grids :: [[String]] -> [Sudoku]
grids gs = map (fromLists . stringsToInts) gs

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n l = let (left, right) = splitAt n l
             in left : (chunks n right)

checkSolution :: Sudoku -> Bool
checkSolution s = all (/=0) s

solveOne :: Sudoku -> Sudoku
solveOne s = (head . fst) (runState (solve s) S.empty)

type Visited = S.Set [Int]
type Solver = State Visited

-- solve
solve :: Sudoku -> Solver [Sudoku]
--solve s | D.trace ("s=" ++ show s) False = undefined
solve s = do -- for the Solver monad
    v <- get
    --put $ S.insert (toList s) $ D.trace ("v=" ++ (show $ length v) ++ "\ns=" ++ (show s)) v
    put $ S.insert (toList s) v
    let moves = solveStep s
    case moves of
        Nothing -> return [s] -- solved
        Just ms -> do
            let dispatched = dispatch ms
            --let dispatched = dispatch (D.trace (show ms) ms)
            --let dispatched = dispatch (D.trace (if (length ms>1) && ((length $ snd $ head ms) == 1) then "OK" else show ms) ms)
            let nexts = map (\(i, move) -> setElem move i s) dispatched
            let notSeen = filter (\n -> S.notMember (toList n) v) nexts
            sols <- mapM solve notSeen
            return $ concat sols
    --let new = setElem move i s
    --return new

solveStep :: Sudoku -> Maybe [((Int, Int), [Int])] -- [((x,y), number)]
solveStep s = let allIndices = [ (i, j) | i <- [1..9], j <- [1..9] ]
                  zeroIndices = filter (\ i -> s ! i == 0) allIndices
              in case zeroIndices of
                [] -> Nothing -- solved
                _ -> let possible = zipMap (\ p -> possibleMoves s p) zeroIndices
                         checked = map (\ (i, m) -> (i, filter (\k -> checkPuzzle (setElem k i s)) m)) possible
                         --checked = map (\ (i, m) -> (i, filter (\k -> checkMove (setElem k i s) i k) m)) possible
                         allowed = filter (not . null . snd) checked
                         allMoves = take 1 $ sortOn (length . snd) allowed
                     in Just allMoves

dispatch :: [((Int, Int), [Int])] -> [((Int, Int), Int)]
dispatch [] = []
dispatch ((x@(p,ms):xs)) = (map (\ m -> (p,m)) ms) ++ dispatch xs

nonNull = filter (/=0)

possibleMoves :: Sudoku -> (Int, Int) -> [Int]
--possibleMoves s (i, j) | D.trace (show s ++ "---" ++ show (i,j)) False = undefined
possibleMoves s p@(i, j) = candidates
    where forbidden = nonZero
          nonZero = nonNull $ concat [inRow, inCol, inSquare]
          inRow = V.toList $ getRow i s
          inCol = V.toList $ getCol j s
          inSquare = toList $ subSquare s i j
          candidates = [1..9] \\ forbidden

allowedAt :: Sudoku -> Int -> (Int, Int) -> Bool
allowedAt s m p@(i, j) = m `notElem` forbidden
    where inRow = V.toList $ getRow i s
          inCol = V.toList $ getCol j s
          inSquare = toList $ subSquare s i j
          forbidden = concat [inRow, inCol, inSquare]

nZerosInCol :: Sudoku -> (Int, Int) -> Int
nZerosInCol s (i, j) = V.length $ V.filter (==0) $ getCol j s

nZerosInRow :: Sudoku -> (Int, Int) -> Int
nZerosInRow s (i, j) = V.length $ V.filter (==0) $ getRow i s

nZerosInSquare :: Sudoku -> (Int, Int) -> Int
nZerosInSquare s (i, j) = length $ filter (==0) inSquare
    where inSquare = toList $ subSquare s i j

checkPuzzle :: Sudoku -> Bool
checkPuzzle s = and $ concat [ansCols, ansRows, ansSquares]
                where ansCols = map (\i -> checkCol (1,i) s) [1..9]
                      ansRows = map (\i -> checkRow (i,1) s) [1..9]
                      ansSquares = map (\i -> checkSquare i s) [(1,1), (4,1), (7,1), (1,4), (4,4), (7,4), (7,1), (7,4), (7,7)]

checkMove :: Sudoku -> (Int, Int) -> Int -> Bool
--checkMove s i m | i==(7,5) && (D.trace (show s ++ "---" ++ show i ++ "---" ++ show m ++ "-->" ++ show ansCol ++ " " ++ show ansRow ++ " " ++ show ansSquare) False) = undefined
--                | otherwise = ans
checkMove s i m = ans
                where s' = setElem m i s
                      ansCol = checkCol i s'
                      ansRow = checkRow i s'
                      ansSquare = checkSquare i s'
                      ans = ansCol && ansRow && ansSquare

missingInV :: V.Vector Int -> [Int]
missingInV v = let nonNull = V.filter (/=0) v
               in [1..9] \\ (V.toList nonNull)

missingInM :: Sudoku -> [Int]
missingInM s = let nonNull = filter (/=0) $ toList s
               in [1..9] \\ nonNull

checkCol :: (Int, Int) -> Sudoku -> Bool
checkCol p s = True
--checkCol p s = checkCol' p s && checkCol'' p s

checkRow :: (Int, Int) -> Sudoku -> Bool
checkRow p s = True
--checkRow p s = checkRow' p s && checkRow'' p s

checkSquare :: (Int, Int) -> Sudoku -> Bool
checkSquare p s = True
--checkSquare p s = checkSquare' p s && checkSquare'' p s

checkCol' :: (Int, Int) -> Sudoku -> Bool
checkCol' p@(_,y) s = let col = getCol y s
                          indices = V.findIndices (==0) col
                          coords = map (\ i -> (i+1,y)) $ V.toList indices
                          rest = nub $ sort $ concatMap (\ c -> possibleMoves s c) coords
                          len = length rest
                          nzeros = nZerosInCol s p
                          in len == nzeros
                          --in len == (D.trace (show len ++ " ==? " ++ show nzeros)) nzeros

checkRow' :: (Int, Int) -> Sudoku -> Bool
checkRow' p@(x,_) s = let row = getRow x s
                          indices = V.findIndices (==0) row
                          coords = map (\ i -> (x,i+1)) $ V.toList indices
                          rest = nub $ sort $ concatMap (\ c -> possibleMoves s c) coords
                          len = length rest
                          nzeros = nZerosInRow s p
                          in len == nzeros
                          --in len == (D.trace (show len ++ " ==? " ++ show nzeros)) nzeros

checkSquare' :: (Int, Int) -> Sudoku -> Bool
checkSquare' p@(x,y) s = let coords = filter (\i -> s ! i==0) $ subSquareIndices x y
                             rest = nub $ sort $ concatMap (\ c -> possibleMoves s c) coords
                             len = length rest
                             nzeros = nZerosInSquare s p
                             in len == nzeros
                             --in len == (D.trace (show len ++ " ==? " ++ show nzeros)) nzeros

checkCol'' :: (Int, Int) -> Sudoku -> Bool
checkCol'' (_,y) s = let col = getCol y s
                         missing = missingInV col
                         mask = map (allowedSomewhereInCol s y) missing
                     in and mask

allowedSomewhereInCol :: Sudoku -> Int -> Int -> Bool
allowedSomewhereInCol s y m = let poss = [ (i,y) | i <- [1..9] ]
                                  mask = map (allowedAt s m) poss
                              in or mask

checkRow'' :: (Int, Int) -> Sudoku -> Bool
checkRow'' (x,_) s = let row = getRow x s
                         missing = missingInV row
                         mask = map (allowedSomewhereInRow s x) missing
                     in and mask

allowedSomewhereInRow :: Sudoku -> Int -> Int -> Bool
allowedSomewhereInRow s x m = let poss = [ (x,i) | i <- [1..9] ]
                                  mask = map (allowedAt s m) poss
                              in or mask

checkSquare'' :: (Int, Int) -> Sudoku -> Bool
checkSquare'' (x,y) s = let square = subSquare s x y
                            missing = missingInM square
                            mask = map (allowedSomewhereInSquare s x y) missing
                        in and mask

allowedSomewhereInSquare :: Sudoku -> Int -> Int -> Int -> Bool
allowedSomewhereInSquare s x y m = let poss = subSquareIndices x y
                                       mask = map (allowedAt s m) poss
                                   in or mask


subSquare :: Sudoku -> Int -> Int -> Sudoku
subSquare s i j = let ((i0,i1),(j0,j1)) = subSquareRange i j
                  in submatrix i0 i1 j0 j1 s

subSquareRange :: Int -> Int -> ((Int, Int), (Int, Int))
subSquareRange i j = let i0 = 3*((i-1) `div` 3) + 1
                         j0 = 3*((j-1) `div` 3) + 1
                         i1 = i0 + 2
                         j1 = j0 + 2
                  in ((i0,i1), (j0,j1))

subSquareIndices :: Int -> Int -> [(Int, Int)]
subSquareIndices x y = let ((i0,i1),(j0,j1)) = subSquareRange x y
                       in [(i,j) | i <- [i0..i1], j <- [j0..j1]]

{-
2  | 8 |3  
 6 | 7 | 84
 3 |56 |2 9
---+---+---
   |1 5|4 8
   |   |   
4 2|7 6|   
---+---+---
3 1|  7| 4 
72 | 4 | 6 
  4| 1 |  3
  -}
