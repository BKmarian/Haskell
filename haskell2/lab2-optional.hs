-- Declarative Programming
-- Lab 2
--

import Data.Char
import Data.List
--import Test.QuickCheck


-- Optional Material

-- 8. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Int -> Int -> [String] -> [String]
crosswordFind ch poz len xs = [x | x <- xs, length x == len && x!!poz == ch]


-- Recursive version
crosswordFindRec :: Char -> Int -> Int -> [String] -> [String]
crosswordFindRec ch poz len [] = []
crosswordFindRec ch poz len (x:xs) | (length x == len) && (x!!poz == ch) = x : crosswordFind ch poz len xs
                                | otherwise = crosswordFind ch poz len xs

-- Mutual test
prop_crosswordFind :: Char -> Int -> Int -> [String] -> Bool
prop_crosswordFind ch poz len list = (crosswordFind ch poz len list) == (crosswordFindRec ch poz len list)



-- 9. search

-- List-comprehension version

search :: String -> Char -> [Int]
search str chr = [i | (x,i) <- zip str [0..], x == chr]

searchRec :: String -> Char -> [Int]
searchRec str chr = searchRec2 str chr 0

searchRec2 :: String -> Char -> Int -> [Int]
searchRec2 [] chr poz = []
searchRec2 (elem:str) chr poz   | (elem == chr) = poz : searchRec2 str chr (poz + 1)
                                | otherwise = searchRec2 str chr (poz + 1)

-- Mutual test
prop_search :: String -> Char -> Bool
prop_search str chr = search str chr == searchRec str chr


-- 10. contains

-- List-comprehension version
contains :: String -> String -> Bool
contains str1 str2 = length [True | x <- (tails str1), isPrefixOf str2 x] > 0

-- Recursive version
containsRec :: String -> String -> Bool
containsRec [] str2 = False
containsRec str str2  | (isPrefixOf str2 str) = True || containsRec (tail str) str2
                      | otherwise = containsRec (tail str) str2

-- Mutual test
prop_contains :: String -> String -> Bool
prop_contains str1 str2 = contains str1 str2 == containsRec str1 str2

