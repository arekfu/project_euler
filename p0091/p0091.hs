newtype Point = P { getCoords :: (Int, Int) } deriving (Ord, Show, Eq)
newtype Direction = D { getComponents :: (Int, Int) } deriving (Ord, Show, Eq)

infixl 7 */
(*/) :: Int -> Direction -> Direction
k */ (D (u, v)) = D (k*u, k*v)

infixl 6 +/
(+/) :: Point -> Direction -> Point
(P (x, y)) +/ (D (u, v)) = P (x + u, y + v)

within :: Int -> Point -> Bool
within n (P (x, y)) = x>=0 && y>=0 && x<=n && y<=n

allOnOrthogonal :: Int -> Point -> [Point]
allOnOrthogonal n p = forw ++ back
    where forw = takeWhile (within n) $ map (\k -> p +/ k */ d) [1..]
          back = takeWhile (within n) $ map (\k -> p +/ k */ d) [-1,-2..]
          d = orthogonal p

withCatheti :: Int -> Int
withCatheti hi = hi^2

withHypotenuse :: Int -> Int
withHypotenuse n = length $ concatMap (allOnOrthogonal n) points
    where points = genPoints n

genPoints :: Int -> [Point]
genPoints n = [ P (x, y) | x <- [0..n], y <- [0..n], x/=0 || y/=0 ]

orthogonal :: Point -> Direction
orthogonal (P (x, y)) = let x' = y
                            y' = -x
                            g = gcd x' y'
                        in D (x' `div` g, y' `div` g)

answer n = withCatheti n + withHypotenuse n

main = print $ answer 2
