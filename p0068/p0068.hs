import Utils (digitsToNumber, isOdd, generatePermutations)
import Data.Ord (comparing)
import Data.List ((\\), union, sort, minimumBy)
import Data.List.Ordered (nub)
import Data.Maybe (catMaybes)
import Splits
import Debug.Trace

data Node = Node { internal :: Int, external :: Int } deriving (Show, Eq, Ord)
data Ring = Ring { nodes :: [Node] } deriving (Show, Eq, Ord)

isValid :: Ring -> Bool
isValid ring = allEqual sums
        where allEqual (x:xs) = all (==x) xs
              sums = ringSums ring

ringSums ring = sums
        where ns = nodes ring
              rSize = length ns
              cns = cycle ns
              sums = take rSize $ nodeSums cns

nodeSums :: [Node] -> [Int]
nodeSums (n1:n2:ns) = (nodeSum n1 n2) : nodeSums (n2:ns)

nodeSum n1 n2 = (internal n1) + (internal n2) + (external n1)

makeRingFromIntExt :: [Int] -> [Int] -> Ring
makeRingFromIntExt ints exts = Ring $ zipWith Node ints exts

makeRingFromBranches :: [[Int]] -> Maybe Ring
makeRingFromBranches branches
        | (sort $ ints `union` exts) == [1..n] = Just $ makeRingFromIntExt ints exts
        | otherwise = Nothing
    where (rints,rexts) = unzip $ map (\ l -> (l!!1, l!!0)) branches
          ints = reverse rints
          exts = reverse rexts
          n = (length ints) + (length exts)


canonicalOrdering :: Ring -> Ring
canonicalOrdering ring = Ring { nodes = can }
        where ns = nodes ring
              rSize = length ns
              cns = cycle ns
              minExtNode = minimumBy (comparing external) ns
              can = take rSize $ dropWhile (/=minExtNode) cns

canonicalRepresentationDigits :: Ring -> [Int]
canonicalRepresentationDigits ring = take (3*rSize) $ repr cns
        where ns = nodes $ canonicalOrdering ring
              rSize = length ns
              cns = cycle ns
              repr (n1:n2:ns) = (external n1) : (internal n1) : (internal n2) : repr (n2:ns)

canonicalRepresentation :: Ring -> Integer
canonicalRepresentation ring = read $ concat $ map show $ canonicalRepresentationDigits ring

minMaxSum n | isOdd n = error "n must be even"
            | otherwise = ( 2*(sumLo*2 + sumHi) `div` n,  2*(sumLo + sumHi*2) `div` n)
            where (lo, hi) = splitAt (n `div` 2) [1..n]
                  sumLo = sum lo
                  sumHi = sum hi

theKSplits rSize tot = concatMap generatePermutations ss
    where rSize2 = rSize * 2
          ss = kSplits 3 [1..rSize2] tot

buildValidRings rSize tot = nub . sort $ map canonicalOrdering $ filter isValid rings
    where ringsAsBranches = buildRings rSize tot
          rings = catMaybes $ map makeRingFromBranches ringsAsBranches

buildRings rSize tot = buildRings' s rSize s []
    where s = theKSplits rSize tot

--buildRings' allS n ss acc | trace (unlines ["***START***",show allS, show n, show ss, show acc,"***STOP***"]) False = undefined
buildRings' _ 0 _ acc = if (s'!!1) == (s!!2) then [acc] else []
    where s = head acc
          s' = last acc
buildRings' _ _ [] _ = []
buildRings' allS n (s:ss) acc = (buildRings' allS (n-1) (allowed s) (s:acc)) ++ (buildRings' allS n ss acc)
    where allowed as = filter (pred as) (allS \\ [s])
          pred as x = (x!!1) == (as!!2)

buildAllValidRings rSize = concatMap (buildValidRings rSize) [s0..s1]
    where (s0, s1) = minMaxSum (2*rSize)

answer = maximum $ filter (\n -> n < 10^16) $ map canonicalRepresentation $ buildAllValidRings 5
