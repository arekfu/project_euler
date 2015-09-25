{-# LANGUAGE FlexibleContexts #-}

import Primes (sumDivisors, makeFactorizer)
import qualified Data.Set as S
import qualified Data.Array as A
import Data.Array.ST
import Control.Monad
import Data.List (findIndex, nub, sortBy)
import Data.Ord (comparing)
--import Debug.Trace
trace a b = b

nMax = 1000000

fact = makeFactorizer nMax

--amicableChainLengths :: A.Array Integer Integer
--amicableChainLengths = runSTArray $ do
--    arr <- newArray (0, nMax) (-1)
--    forM_ [1..nMax] $ \k -> do
--        (left, right) <- breakM (notInArr arr) $ iterate (sumDivisors fact) k
--        let s = head right
--        if s > nMax then
--            forM_ left (\l -> writeArray arr l 0)
--        else do
--            let smallest = minimum left
--            forM_ left (\l -> writeArray arr l smallest)
--    return arr
--
--notInArr arr k = do
--    if k > nMax then
--        return False
--    else do
--        x <- readArray arr k
--        return (x /= (-1))
--
--iterateSumDivisors arr f k set = do
--    let k' = sumDivisors k
--    if k' `S.elem` set then
--        return 

amicableChainLengths :: A.Array Integer (Integer, Integer)
amicableChainLengths = runSTArray $ do
    arr <- newArray (0, nMax) (-1, -1)
    forM_ [1..nMax] $ \k -> do
    --forM_ [1184,1210,1490,2542,5078,3850,7694,8776,8784] $ \k -> do
        res <- minAndCycle arr k
        y <- readArray arr 1184
        case trace (show k ++ " /// " ++ show res) res of
            Nothing -> return ()
            Just (x, l) -> forM_ l $ \k -> writeArray arr k x
    return arr

--oneChain :: Integer -> Maybe (Integer, Integer)
--oneChain n = let a = runSTArray $ do
--                        arr <- newArray (0, nMax) (-1, -1)
--                        res <- minAndCycle arr n
--                        return arr
--             in a A.! n


--minAndCycle :: (MArray a e m) => a Int Int -> Int -> m (Maybe ((Int, Int), [Int])) -- ((min, length), [items to update])
minAndCycle arr n = minAndCycle' n [n] (S.singleton n)
    where minAndCycle' k acc set = do
            let k' = trace (" +++ " ++ show k) $ sumDivisors fact k
            if (k'>nMax) then
                return Nothing
            else if k' `S.member` set then do
                let cyc = k' : (fst $ break (==k') acc)
                return $ Just ((fromIntegral $ minimum cyc, fromIntegral $ length cyc), cyc)
                --return $ trace (show n ++ ": " ++ show cyc ++ " /// " ++ show acc) $ Just ((fromIntegral $ minimum cyc, fromIntegral $ length cyc), cyc)
            else do
                x <- readArray arr k'
                if (fst x)>0 then
                    return $ Just (x, k':acc)
                else do
                    new <- minAndCycle' k' (k':acc) (S.insert k' set)
                    return new

answer = nub $ sortBy (comparing snd) $ filter (\ (a,b) -> a>0) $ A.elems amicableChainLengths

main = print answer

-- findCycle:: (Eq a) => [a] -> [a]
-- findCycle [] = 0
-- findCycle (x:xs) = case firstMatch of
--                         Nothing  -> findCycle xs
--                         (Just n) -> x : (take n xs)
--                         where firstMatch = findIndex (==x) xs

--iters = map (iterate (maybeSumDivisors fact)) [1..nMax]
--
--maybeSumDivisors fact k = let s = sumDivisors fact k in
--                          if s <= nMax then s else k
--
--cycleLengths = map (findCycleLength . take 100) iters

--chain :: Integer -> Maybe [Integer]
--chain n = 
--
--next :: Integer -> Maybe Integer
--next n = let s = sumDivisors fact n
--         in if s<=nMax then Just s else Nothing
