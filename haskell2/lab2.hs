-- Declarative Programming
-- Lab 2
--

import Data.Char
import Data.List
--import Test.QuickCheck



-- 1. halveEvens

-- List-comprehension version

div' :: Int -> Int -> Int
div' x n = x `div` n 
mod' :: Int -> Int -> Int
mod' x n = x `mod` n

halveEvens :: [Int] -> [Int]
halveEvens xs = [div' x 2 | x <- xs, mod' x 2 == 0]

-- Recursive version
halveEvensRec :: [Int] -> [Int]
halveEvensRec [] = []
halveEvensRec (x:xs) | (mod x 2 == 1) = halveEvensRec xs
                     | otherwise = (div' x 2) : halveEvensRec xs

-- Mutual test
prop_halveEvens :: [Int] -> Bool
prop_halveEvens xs = (halveEvensRec xs) == (halveEvens xs)



-- 2. inRange

-- List-comprehension version
inRange :: Int -> Int -> [Int] -> [Int]
inRange lo hi xs = [x | x <- xs, x >= lo && x <= hi]

-- Recursive version
inRangeRec :: Int -> Int -> [Int] -> [Int]

inRangeRec lo hi [] = []
inRangeRec lo hi (x:xs)
                        | (lo <= x && x <= hi) = x : inRangeRec lo hi xs
                        | otherwise = inRangeRec lo hi xs

-- Mutual test
prop_inRange :: Int -> Int -> [Int] -> Bool
prop_inRange lo hi xs = (inRangeRec lo hi xs) == (inRange lo hi xs)



-- 3. sumPositives: sum up all the positive numbers in a list

-- List-comprehension version
countPositives :: [Int] -> Int

countPositives xs = sum [x | x <- xs, x > 0]

-- Recursive version
countPositivesRec :: [Int] -> Int
countPositivesRec [] = 0
countPositivesRec (x:xs)=x + countPositivesRec xs

-- Mutual test
prop_countPositives :: [Int] -> Bool
prop_countPositives list = (countPositivesRec list) == (countPositives list)



-- 4. pennypincher

-- Helper function
discount :: Int -> Int
discount x = fromIntegral x * 10 `div` 100

-- List-comprehension version
pennypincher :: [Int] -> Int
pennypincher xs = sum [discount x | x <- xs, x <= 199]

-- Recursive version
pennypincherRec :: [Int] -> Int
pennypincherRec [] = 0
pennypincherRec (x:xs)   | (x <= 199) = (discount x) + (pennypincherRec xs)
                         | otherwise = pennypincherRec xs

-- Mutual test
prop_pennypincher :: [Int] -> Bool
prop_pennypincher xs = (pennypincherRec xs) == (pennypincher xs)



-- 5. sumDigits

-- List-comprehension version
multDigits :: String -> Int
multDigits xs = sum [digitToInt x | x <- xs, isDigit x]

-- Recursive version
multDigitsRec :: String -> Int
multDigitsRec [] = 0
multDigitsRec (x:xs)   | (isDigit x == True) = (digitToInt x) + (multDigitsRec xs)
                       | otherwise = multDigitsRec xs

-- Mutual test
prop_multDigits :: String -> Bool
prop_multDigits xs = (multDigitsRec xs) == (multDigits xs)



-- 6. capitalise

-- List-comprehension version

capitalise :: String -> String
capitalise (head:xs) = [toUpper head] ++ [toLower x | x <- xs]

-- Recursive version

capitaliseRec2 :: String -> String
capitaliseRec2 [] = []
capitaliseRec2 (el:xs) = [toLower el] ++ capitaliseRec2 xs

capitaliseRec :: String -> String
capitaliseRec (el:xs) = [toUpper el] ++ capitaliseRec2 xs

-- Mutual test
prop_capitalise :: String -> Bool
prop_capitalise xs = (capitaliseRec xs) == (capitalise xs)



-- 7. title

-- List-comprehension version
title :: [String] -> [String]
title (head:xs) = [capitalise head] ++ [if length x >= 4 then capitalise x else x| x <- xs]

titleRec :: [String] -> [String]

titleRec [] = []
titleRec (head:xs) = capitalise head : titleRec2 xs

titleRec2 :: [String] -> [String]

titleRec2 [] = []
titleRec2 (head:xs)     | (length head < 4) = head : titleRec2 xs
                        | otherwise = (capitalise head) : titleRec2 xs

-- mutual test
prop_title :: [String] -> Bool
prop_title xs = (title xs) == (titleRec xs)

