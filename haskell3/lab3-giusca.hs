-- Declarative Programming
-- Lab 3
--


import Data.Char
import Data.List
import Test.QuickCheck
import Data.Set

-- 1.
rotate ::  Int -> [Char] -> [Char]
rotate n x = if (n < 0 || n > length x)  
             then error "Wrong number"
			 else [chr(ord(a) + n)|a <- x]
-- 2.
prop_rotate :: Int -> String -> Bool
prop_rotate k str = rotate (l - m) (rotate m str) == str
                        where l = length str
                              m = if l == 0 then 0 else k `mod` l

-- 3. 
makeKey :: Int -> [Char] -> [(Char, Char)]
makeKey n x = zip x (rotate n x)
-- 4.
lookUp :: Char -> [(Char, Char)] -> Char
lookUp a [] = a
lookUp a ((x1,x2):xs) = if a == x1
                        then x2
						else lookUp a xs

-- 5.
encipher :: Int -> Char -> Char
encipher n c = if n > 26
               then error "Number too big"
			   else if ord c >= ord 'A' && ord c <= ord 'Z'
			        then if ord c + n > ord 'Z'
					     then chr(ord 'A' + ord c + n - ord 'Z' -1)
						 else chr(ord(c) + n)
					else if ord c >= ord 'a' && ord c <= ord 'z'
					     then if ord c + n > ord 'z'
						      then chr(ord 'a' + ord c + n - ord 'z' -1)
							  else chr(ord c + n)
					     else c
						 

-- 6.
normalize :: String -> String
normalize [] = []
normalize (x:xs) = if ((ord x >= ord 'A' && ord x <= ord 'z') || isDigit x)
                   then [toUpper(x)] ++ normalize xs
				   else normalize xs

-- 7.
encipherStr :: Int -> String -> String
encipherStr n x = textCriptat n (normalize x)

textCriptat :: Int -> String -> String
textCriptat n x = [encipher n a | a <- x]


-- 8.
reverseKey :: [(Char, Char)] -> [(Char, Char)]
reverseKey [] = []
reverseKey ((a1,a2):x1) = [(a2,a1)] ++ reverseKey x1

-- 9.
decipher :: Int -> Char -> Char
decipher n c = if n > 26
               then error "Number too big"
			   else if ord c >= ord 'A' && ord c <= ord 'Z'
			        then if ord c - n < ord 'A'
					     then chr(ord 'Z' + ord c - n - ord 'A' +1)
						 else chr(ord(c) - n)
					else if ord c >= ord 'a' && ord c <= ord 'z'
					     then if ord c - n < ord 'a'
						      then chr(ord 'z' + ord c - n - ord 'z' +1)
							  else chr(ord c - n)
					     else c

decipherStr :: Int -> String -> String
decipherStr n x = stergeElemente [decipher n a | a <- x]

stergeElemente :: [Char] -> [Char]
stergeElemente [] = []
stergeElemente (x:xs) = if (isUpper x || isDigit x || x == ' ')
                        then [x] ++ stergeElemente xs
						else stergeElemente xs

-- 10.
prop_cipher :: Int -> String -> Bool
prop_cipher = undefined

-- 11.
contains :: String -> String -> Bool
contains x1 x2 = if x1 == []
                 then False
				 else if length x1 >= length x2 && verifica (splitAt (length x2) x1) x2
                      then True
				      else contains (removeOne x1) x2

removeOne :: String -> String
removeOne [] = []
removeOne (x:xs) = xs

verifica :: ([Char],[Char]) -> [Char]-> Bool
verifica (x1 , x2) x3  = if x1 == x3 then True else False


-- 12.
candidates :: String -> [(Int, String)]
candidates x = simulateFor 1 x

simulateFor :: Int -> String -> [(Int, String)]
simulateFor i x = if i == 27
                  then []
				  else if (contains (decipherStr i x) "AND") || (contains (decipherStr i x) "THE")
                       then [(i, decipherStr i x)] ++ simulateFor (i+1) x
				       else simulateFor (i+1) x

-- Optional Material

-- 13.
splitEachFive :: String -> [String]
splitEachFive [] = []
splitEachFive x = if length x > 5
                  then param1 (splitAt 5 x) : splitEachFive(param2 (splitAt 5 x))
				  else (x ++ (replicate (5-length x) 'X')) : splitEachFive []
				  
param1 :: ([Char],[Char]) -> [Char]
param1 (x1,x2) = x1
param2 :: ([Char],[Char]) -> [Char]
param2 (x1,x2) = x2
-- 14.
prop_transpose :: String -> Bool
prop_transpose x = splitEachFive x == transpose (transpose (splitEachFive x))

-- 15.
encrypt :: Int -> String -> [String]
encrypt n x = transpose(splitEachFive(encipherStr n x))

-- 16.
decrypt :: Int -> [String] -> String
decrypt n x = decipherStr n (unsplit(transpose x))

unsplit :: [String] -> String
unsplit [] = []
unsplit (x:xs) = x ++ unsplit xs

-- Challenge (Optional)

-- 17.
countFreqs :: String -> [(Char, Int)]
countFreqs x = zip (nub x) (numaraFrecvente (nub x) (nub x))

numaraFrecvente :: String -> String -> [Int]
numaraFrecvente x_tot [] = []
numaraFrecvente x_tot (x:xs) = numara x x_tot : numaraFrecvente x_tot xs

numara :: Char -> String -> Int
numara x [] = 0
numara x (x1:xs) = if x == x1
                   then 1 + numara x xs
				   else numara x xs

-- 18
freqDecipher :: String -> [String]
freqDecipher = undefined















 
