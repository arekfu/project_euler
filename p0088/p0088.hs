{-# LANGUAGE ViewPatterns #-}

import qualified Data.Sequence as S
import Data.Sequence ((<|), (><), Seq)
import Data.Sequence (ViewL((:<), EmptyL), viewl)
import Data.Foldable (foldl', toList)
import Utils (zipMap)
import Data.List.Ordered (nub, sort)

newtype SeqWithIndex = SWI { getSWI :: (Seq Int, Int, Int, Int) } deriving (Eq, Ord, Show)

increment :: Integral a => SeqWithIndex -> [SeqWithIndex]
increment (SWI (v,i,s,_)) = inc (S.drop i v) i (if i==0 then 0 else S.index v (i-1))
    where   inc (viewl -> EmptyL) _ _ = []
            inc (viewl -> x :< xs) n prev =
                if x == prev
                then inc xs (n+1) x
                else let (left, right) = S.splitAt n v
                         newSeq = left >< (S.adjust succ 0 right)
                         newProd = prodSeq newSeq
                     in if newProd <= newSum
                        then (SWI (newSeq, n, newSum, newProd)) : (inc xs (n+1) x)
                        else inc xs (n+1) x
            newSum = succ s

--increment :: Integral a => Seq a -> [Seq a]
--increment v = inc (mask v) 0
--    where inc (viewl -> b :< bs) n =
--                         if b
--                         then let (left, right) = S.splitAt n v
--                                  newSeq = left >< (S.adjust succ 0 right)
--                              in newSeq : (inc bs (n+1))
--                         else inc bs (n+1)
--          inc (viewl -> EmptyL) _ = []

--mask :: Eq a => Seq a -> [Bool]
--mask l = True : (toList $ S.zipWith (/=) l $ S.drop 1 l)

--mask :: Eq a => Seq a -> Seq Bool
--mask l = True <| (S.zipWith (/=) l $ S.drop 1 l)

-- generates sets of integers with increasing sum
sets :: Int -> [SeqWithIndex]
sets k = concat $ iterate (concatMap increment) [SWI (S.replicate k 1, 0, k, 1)]

--sumSeq :: Num a => Seq a -> a
--sumSeq s = foldl' (+) 0 s
sumSeq :: Seq Int -> Int
sumSeq s = (foldl' (+) 0 left) + (S.length right)
    where (left, right) = S.spanl (/=1) s

--prodSeq :: Num a => Seq a -> a
--prodSeq s = foldl' (*) 1 s
prodSeq :: (Eq a, Num a) => Seq a -> a
prodSeq s = foldl' (*) 1 $ S.takeWhileL (/=1) s

solution :: Int -> SeqWithIndex
solution k = head $ dropWhile (\(SWI (seq, _, s, p)) -> p /= s) ss
          where ss = sets k

answersUpTo :: Int -> [(Int, Int)]
answersUpTo k = map (\(i, SWI (_,_,_,p)) -> (i,p)) $ zipMap solution [2..k]

main = do
        let ans = answersUpTo 1200
        mapM_ print ans
        let tot = sum $ nub $ sort $ map snd ans
        print tot
