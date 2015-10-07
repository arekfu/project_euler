import Data.List (foldl', sort)
import Data.List.Split
import qualified Data.Map.Strict as M
import Utils (pairs, numberToDigits, digitsToNumber)
import Control.Monad.Reader
import Data.Maybe
import Data.Tuple (swap)

main = do
    text <- readFile "p098_words.txt"
    let ws = split (dropDelims $ dropBlanks $ oneOf "\",") text
    let families = filterTrivialAnagrams $ mapFromWords ws
    print families
    putStrLn $ "families: " ++ (show $ length families)
    let answer = findLargestSquare families
    putStrLn $ "answer: " ++ (show answer)

type AnagramMap = M.Map String [String]
type SquaresMap = M.Map Int [Int]

mapFromWords :: [String] -> AnagramMap
mapFromWords ws = foldl' (\ map word -> M.insertWith (++) (sort word) [word] map) M.empty ws

filterTrivialAnagrams :: AnagramMap -> AnagramMap
filterTrivialAnagrams map = M.filter (\l -> length l>1) map

makeSquaresMap :: Int -> SquaresMap
makeSquaresMap upto = foldl' (\map k -> M.insert k (kDigitSquares k) map) M.empty [1..upto]

allSquares = map (^2) [1..]

kDigitSquares :: Int -> [Int]
kDigitSquares k = takeWhile (<10^k) $ dropWhile (<10^(k-1)) allSquares

findLargestSquare :: AnagramMap -> Int
findLargestSquare map =
    let maxWordLength = maximum $ M.map (length . head) map
        squaresMap = makeSquaresMap maxWordLength
    in maximum $ M.mapMaybe (\ m -> runReader (findLargestSquareForList m) squaresMap) map

findLargestSquareForList :: [String] -> Reader SquaresMap (Maybe Int)
findLargestSquareForList l = do
    values <- mapM findLargestSquareForPair $ pairs l
    return $ maximum values

findLargestSquareForPair :: (String, String) -> Reader SquaresMap (Maybe Int)
findLargestSquareForPair p@(s1, s2) = do
    let len = length s1
    squaresMap <- ask
    let squares = squaresMap M.! len
    let valid = map (checkSquareForPair p squares) squares
    return $ maximum valid

checkSquareForPair :: (String, String) -> [Int] -> Int -> Maybe Int
checkSquareForPair (s1, s2) squares n =
    let digits = numberToDigits n
        assoc = zip s1 digits
        valid = validateAssocList assoc && validateAssocList (map swap assoc)
        other = translate assoc s2
    in if valid
       then if other `elem` squares then Just $ max n other else Nothing
       else Nothing


validateAssocList :: (Eq a, Eq b) => [(a, b)] -> Bool
validateAssocList [] = True
validateAssocList ((c,d):xs) =
    let rest = validateAssocList xs
    in case lookup c xs of
        Nothing -> rest
        Just d' -> (d == d') && rest

translate :: [(Char, Int)] -> String -> Int
translate assoc s = digitsToNumber $ catMaybes $ map (\c -> lookup c assoc) s
