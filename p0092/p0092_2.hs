import Data.Array.ST (runSTArray, newArray, writeArray, readArray)
import Data.Array (Array, elems, inRange)
import Control.Monad (forM_, mapM_, liftM2)
import Control.Monad.ST (runST)

sumSquareDigit :: Int -> Int
--sumSquareDigit n = sum $ map (^2) $ numberToDigitsBackwards n
sumSquareDigit n | n<10 = n^2
                 | otherwise = (n `mod` 10)^2 + sumSquareDigit (n `div` 10)

solveUpTo :: Int -> Array Int Int
solveUpTo n = runSTArray $ do
    let arrRange = (1, 2*n)
    arr <- newArray arrRange 0
    writeArray arr 1 1
    writeArray arr 89 89
    forM_ [2..n] $ \k -> do
       (ks, rest) <- breakM (mpred arr) $ iterate sumSquareDigit k
       s <- readArray arr $ head rest
       mapM_ (\l -> if inRange arrRange l then writeArray arr l s else return ()) ks
    return arr

mpred arr l = do
    x <- readArray arr l
    return (x /= 0)

breakM :: (Eq a, Monad m) => (a -> m Bool) -> [a] -> m ([a], [a])
breakM _ xs@[] = return (xs, xs)
breakM predM l@(x:xs) = do
    val <- predM x
    case val of
        True -> return ([], l)
        _ -> do
                (ys, zs) <- breakM predM xs
                return (x:ys, zs)

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ xs@[] = return xs
takeWhileM predM l@(x:xs) = do
    val <- predM x
    case val of
        False -> return []
        _ -> do
                ys <- takeWhileM predM xs
                return (x:ys)

target n = head $ dropWhile (\k -> k /= 1 && k /= 89) $ iterate sumSquareDigit n

solve n = take n $ elems $ solveUpTo n
--solve n = runState (solveUpTo n) emptyState

correct n = monadic == fp
    where monadic = answer n
          fp = length $ filter (\k -> k <= n && target k == 89) [1..n]

answer = length . filter (==89) . solve
main = print $ answer 10000000
