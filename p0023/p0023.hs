import Data.Array
import Data.List (group)
import Debug.Trace
import qualified Data.Set as Set

abundantNumbers = filter isAbundant [12..nmax+5]

sum2AbundantNumbers = set
                where set = Set.fromList [ x + y | x <- abundantNumbers, y <- abundantNumbers, x + y <= nmax ]

complement = Set.difference naturals sum2AbundantNumbers
        where naturals = Set.fromList [1..nmax]

sumComplement = sum $ Set.toList complement
