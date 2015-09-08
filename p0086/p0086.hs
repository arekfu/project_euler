-- the shortest path has length
-- dmin = \sqrt{c^2+(a+b)^2}

import Pythagorean
import Utils (zipMap)

snd3 (_,x,_) = x
swap12 (x,y,z) = [(x,y,z), (y,x,z)]

solutionTriples cmax = filter (\(_,c,_) -> c<=cmax) tooMany
    where tooMany = concatMap swap12 $ allTriples (\(aPlusB, c, _) -> c<=cmax || aPlusB<=cmax)

countSplits (aPlusB, c, _) = if lo <= hi then hi - lo + 1 else 0
    where lo = max (aPlusB - c) 1
          aPlusBOver2 = aPlusB `div` 2
          hi = aPlusBOver2

countAllSplits (x, y, z) = countSplits (x, y, z) + countSplits (y, x, z)

solution cmax = sum $ map countSplits $ solutionTriples cmax

-- answer found by manual bisection
answer = (solution 1817, solution 1818)

main = print answer
