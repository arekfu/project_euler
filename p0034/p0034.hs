import Utils
answer = goodPairs
        where interval = [3..10000000]
              asDigits = map numberToDigits interval
              sumFactorials = foldl (\a -> \b -> a + (factorial b)) 0
              sums = map sumFactorials asDigits
              pairs = zip interval sums
              goodPairs = filter (\(a,b) -> a==b) pairs
