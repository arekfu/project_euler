import Text.Parsec

data Point = P { getX :: Int, getY :: Int } deriving (Show, Ord, Eq)
data Vect = V { getVX :: Int, getVY :: Int } deriving (Show, Ord, Eq)
data Triangle = T { getP1 :: Point, getP2 :: Point, getP3 :: Point } deriving (Show, Ord, Eq)

(~-~) :: Point -> Point -> Vect
(P x0 y0) ~-~ (P x1 y1) = V (x0-x1) (y0-y1)

(~*~) :: Vect -> Vect -> Int
(V x0 y0) ~*~ (V x1 y1) = x0*x1 + y0*y1

(~^~) :: Vect -> Vect -> Int
(V x0 y0) ~^~ (V x1 y1) = x0*y1 - x1*y0

origin = P 0 0

-- parsing

parseTriangles :: String -> Either ParseError [Triangle]
parseTriangles input = parse triangles "(triangles)" input

triangles = endBy triangle eol
eol = char '\n'

triangle = do
    p1 <- point
    char ','
    p2 <- point
    char ','
    p3 <- point
    return $ T p1 p2 p3

point = do
    x <- signedInteger
    char ','
    y <- signedInteger
    return $ P x y

signedInteger = do
    s <- oneOf "-0123456789"
    ds <- many digit
    return $ read (s:ds)

-- end of parsing

main = do
    text <- readFile "p102_triangles.txt"
    case parseTriangles text of
        Left err -> print err
        Right triangles -> do
            let answer = process triangles
            print answer

process triangles = length $ filter (contains origin) triangles

-- check if P0 and P1 are on the same side of the AB line
sameSide p0 p1 pa pb = let va0 = p0 ~-~ pa
                           va1 = p1 ~-~ pa
                           vab = pb ~-~ pa
                           abc0 = vab ~^~ va0
                           abc1 = vab ~^~ va1
                        in (abc0<0 && abc1<0) || (abc0>0 && abc1>0)

contains point (T p1 p2 p3) = sameSide point p1 p2 p3 && sameSide point p2 p3 p1 && sameSide point p3 p1 p2

