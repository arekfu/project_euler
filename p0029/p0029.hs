import Data.List
range = [2..100]

powers = [ x^y | x <- range, y <- range ]

nDistinctPowers = length $ group $ sort powers
