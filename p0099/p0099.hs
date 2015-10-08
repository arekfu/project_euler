import Text.Parsec
import Data.List (maximumBy)

main = do
    text <- readFile "p099_base_exp.txt"
    case parseBaseExps text of
        Left err -> print err
        Right pairs -> do
            let answer = process pairs
            print answer

parseBaseExps :: String -> Either ParseError [(Integer, Integer)]
parseBaseExps input = parse baseExps "(baseExp)" input

baseExps = sepBy baseExp eol
eol = char '\n'

baseExp = do
    x <- integer
    char ','
    y <- integer
    return (x, y)

integer = do
    ds <- many digit
    return $ read ds

--- parsing ends here

process :: [(Integer, Integer)] -> Int
process pairs = let results = map (\ (a, b) -> (fromIntegral b) * (log (fromIntegral a))) pairs
                    lns = zip results [1..]
                in snd $ maximumBy (\(a,_) (b,_) -> compare a b) lns
