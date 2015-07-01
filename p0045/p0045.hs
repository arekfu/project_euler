import Utils

triangular = map (\n -> n * (n+1) `div` 2) [1..]

answer = take 3 $ filter (\n -> isPentagonal n && isHexagonal n) triangular

