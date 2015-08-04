import Data.List
import Data.List.Ordered (isSorted)
import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S

main = do
         input <- readFile "p079_keylog.txt"
         let answer = process input
         print answer

process input = shortest $ nub $ lines input

shortest input = (solution, checkSolution solution input)
    where solution = (reverse . charSeq . makeMap) input

type CharSet = S.Set Char
type FollowersMap = M.Map Char CharSet

pairs :: String -> [(Char, CharSet)]
pairs string = zipWith (\c1 c2 -> (c1, S.singleton c2)) string (tail string)

allPairs :: [String] -> [(Char, CharSet)]
allPairs ls = concatMap pairs ls

makeMap :: [String] -> FollowersMap
makeMap ls = foldr (\(k, v) m -> M.insertWith S.union k v m) initialMap $ allPairs ls
    where initialMap = M.fromList $ zip (S.elems $ allValues ls) (repeat S.empty)

allValues :: [String] -> CharSet
allValues ss = S.unions $ map S.fromList ss

peelLastKey :: FollowersMap -> (Char, FollowersMap)
peelLastKey m = (lastChar, newMap)
    where lastChar  = fst $ M.elemAt 0 $ M.filter null m
          diffMap   = M.delete lastChar m
          newMap    = M.map (\ s -> S.difference s $ S.singleton lastChar) diffMap

charSeq :: FollowersMap -> [Char]
charSeq m | M.null m = []
          | otherwise = c : charSeq m'
            where (c, m') = peelLastKey m

checkSolution :: [Char] -> [String] -> [String]
checkSolution code input = dropWhile (isValidSubCode code) input

isValidSubCode code subcode = ((length maybeIndices)==3) && isSorted indices
    where maybeIndices = map (\c -> elemIndex c code) subcode
          indices = catMaybes maybeIndices
