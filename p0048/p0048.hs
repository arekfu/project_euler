nums = [1..1000]
pows = zipWith (^) nums nums
rests = map (`mod` 10^10) pows
answer = (sum rests) `mod` 10^10
