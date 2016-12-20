-- Declarative Programming
-- Lab 3
--


import Data.Char
import Data.List
--import Test.QuickCheck


-- 1.
rotate :: Int -> [Char] -> [Char]
rotate n list = drop n list ++ take n list 

-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
temp = ['a'..'z']
makeKey :: Int -> [(Char, Char)]
makeKey n = zip temp (rotate n temp)

-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp char lista = [i | (x,i) <- lista, x == char]!!0

-- 5.
encipher :: Int -> Char -> String -> Char

encipher nr char [] = 'a'
encipher nr char (head:temp) | (head == char) = temp!!(nr - 1) 
                             | otherwise = encipher nr char temp

-- 6.
normalize :: String -> String
normalize xs = [if isLetter x then toUpper x else x | x <- xs , isLetter x || isDigit x]

-- 7.
encipherStr :: Int -> String -> String
encipherStr nr str = [encipher nr x temp | x <- (normalize str)]

-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey xs = [ (y,x) | (x,y) <- xs] 

-- 9.
decipher :: Int -> Char -> Char
decipher nr char = if ord 'A' <=  (ord char - nr) then chr (ord char - nr) else chr (ord 'Z' - (nr + 65 - ord char - 1)) 

decipherStr :: Int -> String -> String
decipherStr nr str = [if isUpper x then decipher nr x else x | x <- str, isLower x == False]

-- 10.
prop_cipher :: Int -> String -> Bool
prop_cipher = undefined

-- 11.
contains :: String -> String -> Bool
contains str1 str2 = length [True | x <- (tails str1), isPrefixOf str2 x] > 0

-- 12.
candidates :: String -> [(Int, String)]
candidates str = [(nr ,(decipherStr nr str)) | nr <- [1..26] , contains (decipherStr nr str) "THE" || contains (decipherStr nr str) "AND"]

-- Optional Material

-- 13.
-- splitEachFive :: String -> [String]
-- splitEachFive str = [ if length (take 5 str) == 5 take 5 str else  (take 5 str) ++ replicate ( length (take 5 str) 'X') | str <- drop 1 str]

splitEachFive :: String -> [String]
splitEachFive [] = []
splitEachFive str   | length (take 5 str) == 5  = (take 5 str) : splitEachFive(drop 5 str)
                    | otherwise = ((take 5 str) ++ (replicate ( 5 - length (take 5 str)) 'X')) : splitEachFive(drop 5 str) 

-- 14.
prop_transpose :: String -> Bool
prop_transpose x = splitEachFive x == transpose (transpose (splitEachFive x))
-- 15.
encrypt :: Int -> String -> String
encrypt nr str = concat (transpose (splitEachFive(encipherStr nr str)))

-- 16.
decrypt :: Int -> String -> String
decrypt nr str = concat(transpose( splitEachFive (decipherStr nr (removeX str))))

removeX :: String -> String
removeX str = if (str!!(length str - 1)) == 'X' then fst (splitAt (length str - 1) str) else str 

-- Challenge (Optional)

-- 17.
countFreqs :: String -> [(Char, Int)]
countFreqs = undefined

-- 18
freqDecipher :: String -> [String]
freqDecipher = undefined