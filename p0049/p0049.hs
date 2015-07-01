import qualified Data.Set as Set
import Data.List
import Primes
import Utils

fourDigitPrimes = filter (\n -> n>999 && n<9999) $ take 2000 primeTable

primePermutations n = Set.toList $ Set.fromList [ perm | perm <- generateDigitPermutations n, isPrime perm ]

diffs l = [ (n2-n1, n1, n2, n3) | i <- [0..n1], j <- [(i+1)..n1], k <- [(j+1)..n1], let n1=l !! i, let n2=l !! j, let n3=l !! k, n2-n1==n3-n2]
        where n1 = (length l)-1

nSeries n = groupedDiffs
        where perms = primePermutations n
              ds = diffs perms
              sortedDiffs = sort ds
              groupedDiffs = groupBy (\(a,b,c,d)-> \(e,f,g,h) -> a==e) sortedDiffs
              lengthSeries = map length groupedDiffs

answers = map nSeries fourDigitPrimes
