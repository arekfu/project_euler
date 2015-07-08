import Data.Char
import Data.Bits
import Data.List

main = do
        text <- readFile "p059_cipher.txt"
        decryptLoop $ convertToChars text
        return ()

decryptLoop text = do
                putStrLn "Enter char for decryption classification (space works well):"
                chars <- getLine
                let char = head chars
                let best0 = nBestDecryptions 10 char 0 text
                let best1 = nBestDecryptions 10 char 1 text
                let best2 = nBestDecryptions 10 char 2 text
                putStrLn "i=0 decryptions:"
                print best0
                putStrLn "i=1 decryptions:"
                print best1
                putStrLn "i=2 decryptions:"
                print best2
                putStrLn "Enter decrypt key:"
                key <- getLine
                putStrLn $ "key: " ++ key
                let decrypted = decryptText key text
                putStrLn $ "Decrypted text: " ++ decrypted
                let answer = sum $ map ord decrypted
                putStrLn $ "Answer = " ++ show answer
                decryptLoop text

nBestDecryptions n char i charText = bestDecryption n char $ ithChars i charText

convertToChars :: String -> String
convertToChars text = map (chr . read) $ words $ replace ',' ' ' text

replace :: Char -> Char -> String -> String
replace from to string = map (replaceChar from to) string

replaceChar :: Char -> Char -> Char -> Char
replaceChar a b c = if a==c then b else c

decryptChar :: Char -> Char -> Char
decryptChar key encrypted = chr $ (ord key) `xor` (ord encrypted)

ithChars i text = map (text !!) [i,(i+3)..(n-1)]
        where n=length text

decryptText :: String -> String -> String
decryptText key encrypted = zipWith decryptChar (cycle key) encrypted

count :: Char -> String -> Int
count c string = length $ filter (\s -> s==c) string

allDecryptions char text = [ (count char $ decryptText [k] text, k) | k <- ['a'..'z'] ]

bestDecryption n char text = take n $ reverse $ sort $ allDecryptions char text

