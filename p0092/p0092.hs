import Utils
import qualified Data.Set as S
import Control.Monad.State
import Debug.Trace
import Data.List (foldl')

nmax = 1000000

sumSquareDigit :: Int -> Int
--sumSquareDigit n = sum $ map (^2) $ numberToDigitsBackwards n
sumSquareDigit n | n<10 = n^2
                 | otherwise = (n `mod` 10)^2 + sumSquareDigit (n `div` 10)

type SolutionSet1 = S.Set Int
type SolutionSet89 = S.Set Int

emptyState = (S.singleton 1, S.singleton 89)

--solveUpTo :: Int -> State (SolutionSet1, SolutionSet89) Int
--solveUpTo n = do
--    forM_ [1..n] $ \k -> do
--        (sol1, sol89) <- get
--        put $ update (sol1, sol89) k
--    (sol1'', sol89'') <- get
--    return $ length sol89''

solveUpTo :: Int -> (SolutionSet1, SolutionSet89)
solveUpTo n = foldl' update emptyState [2..n]

update :: (SolutionSet1, SolutionSet89) -> Int -> (SolutionSet1, SolutionSet89)
update (s1, s89) n = let pred k = k `S.member` s89 || k `S.member` s1
                         (ks, rest) = break pred $ iterate sumSquareDigit n
                         s = head rest
                         ins1 = s `S.member` s1
                         s1' = if ins1 then foldl' (flip S.insert) s1 ks else s1
                         s89' = if (not ins1) then foldl' (flip S.insert) s89 ks else s89
                     in (s1', s89')

target n = head $ dropWhile (\k -> k /= 1 && k /= 89) $ iterate sumSquareDigit n

solve n = length $ fst $ solveUpTo n
--solve n = runState (solveUpTo n) emptyState

--correct n = monadic == fp
--    where monadic = filter (<=n) $ S.toList $ snd $ snd $ solve n
--          fp = filter (\k -> k <= n && target k == 89) [1..n]
--
--answer n = fst $ solve n

answer = solve
main = print $ answer 1000000
