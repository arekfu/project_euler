import Utils (cartProd)
import Data.List
import qualified Data.Set as Set

theDigits = [1..9]

makeNDigitLists :: Integer -> [Integer] -> [[Integer]]
makeNDigitLists n dig
        | n>(fromIntegral (length dig)) = error "n must be <= length dig"
        | n==1 = map (\x -> [x]) dig
        | otherwise = [ x:y | x <- dig, let remaining=(delete x dig), y <- (makeNDigitLists (n-1) remaining) ]

generatePermutations :: [Integer] -> [[Integer]]
generatePermutations dig = makeNDigitLists n dig
        where n = fromIntegral $ length dig

chunks2 :: Int -> Int -> [a] -> ([a],[a],[a])
chunks2 i j l = (first,second,third)
        where (first,secth) = splitAt i l
              (second,third) = splitAt j secth

allSplits dig = concat $ [ map (chunks2 n1 n2) perms | n1 <- [n1Min..n1Max], n2 <- [1..((length dig)-1)] ]
        where n = fromIntegral $ length dig
              n1Min = 1
              n1Max = (length dig) - 2
              perms = generatePermutations dig

chunkToNumbers :: (Num a) => [a] -> a
chunkToNumbers [] = 0
chunkToNumbers (x:xs) = (x*10^(length xs)) + chunkToNumbers xs

chunkTripletToNumbers (a,b,c) = (chunkToNumbers a, chunkToNumbers b, chunkToNumbers c)

allTriplets dig = tuples
        where splits = allSplits dig
              tuples = map chunkTripletToNumbers splits

validTriplets dig = filter (\(a,b,c)-> a*b==c)  trip
        where trip = allTriplets dig

validProducts dig = Set.fromList (map (\(a,b,c)->c) $ validTriplets dig)

totalProduct dig = Set.foldl (+) 0 $ validProducts dig

main = do
        print $ validTriplets theDigits
        print $ validProducts theDigits
        print $ totalProduct theDigits
