nDigitNumbers n = let low = 10^(n-1)
                      hi = 10^n
                      in takeWhile (<hi) $ dropWhile (<low) $ map (^n) [1..]
        
answer = length $ concatMap nDigitNumbers [1..50]

main = print answer
