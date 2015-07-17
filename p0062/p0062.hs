import Data.List
import Utils

cubes = map (^3) [1..]

cubeDigits = zip cubes $ map (sort . numberToDigits) cubes

nextPermutationOf ((n,digits):xs) = (n : (map fst $ filter (\ (m,ds) -> ds==digits) candidates)) : nextPermutationOf xs
        where l = length digits
              candidates = takeWhile (\ (m,ds) -> m<10^l) xs

answer = head $ dropWhile (\ l -> (length l)<5) $ nextPermutationOf cubeDigits
