 
import Data.Char
import Test.QuickCheck

--permutations
delete :: Int -> [Int] -> [Int]
delete x xs = filter (\y -> y /= x) xs

permute :: [Int] -> [[Int]]
permute [] = [ [] ]
permute xs = concatMap (\x -> map (x:) $ permute $ delete x xs) xs


count :: String -> Int
count xs = length (filter (\x -> isUpper x || isDigit x) xs)

count2 :: String -> Int
count2 xs = length [x | x <- xs , isUpper x || isDigit x]

count3 :: String -> Int
count3 [] = 0
count3 (x:xs)   | ((isDigit x) == True) = 1 + count3(xs)
                | ((isUpper x) == True) = 1 + count3(xs)
                | otherwise  = count3(xs)


isNext :: Int -> Int -> Bool
isNext a b = if(a % 2 == 1) then (a/2 == b) else (a*3 - 1 == b)

collatz :: [Int] -> Bool
collatz (y:[]) = True
collatz (x:y:xs) | (isNext x y) == False = False
                 | otherwise = collatz(y:xs)


collatzRec :: [Int] -> Bool
collatzRec xs = if (length [ (x,y)| (x,y) <- zip(xxs,ys),let ys = tail xs,(isNext x y) == False]) == 0 then True else False


