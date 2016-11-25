-- Informatics 1 - Functional Programming 
-- Tutorial 3
--
-- Week 5 - Due: 22/23 Oct.

import Data.Char
import Test.QuickCheck
import Data.List


-- 1. Map
-- a. (4 simboluri)
uppers :: String -> String
uppers = map toUpper

-- b. (7 simboluri)
doubles :: [Int] -> [Int]
doubles = map dub where dub x = x * x

-- c. (10 simboluri)
penceToPounds :: [Int] -> [Float]
penceToPounds xs = map topound xs where topound x = (fromIntegral x / 100)

-- d. (11 simboluri)
uppers' :: String -> String
uppers' xs = [ toUpper x | x <- xs]

-- (8 simboluri)
prop_uppers :: String -> Bool
prop_uppers xs = uppers' xs == uppers xs


maxim :: [String] -> Int
maxim xs = foldr (\x y -> if x > y then x else y) 0 (map (\x -> length x) xs)

-- 2. Filter
-- a. (4 simboluri)
alphas :: String -> String
alphas = filter isdigitorletter 

isdigitorletter :: Char -> Bool
isdigitorletter ch = isDigit ch || isLetter ch
-- b. (8 simboluri)
rmChar ::  Char -> String -> String
rmChar ch = filter (\x -> x /= ch)

-- c. (8 simboluri)
above :: Int -> [Int] -> [Int]
above x = filter (\y -> y /= x)

-- d. (13 simboluri)
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals = filter (\(x,y) -> x /= y)

-- e. (15 simboluri)
rmCharComp :: Char -> String -> String
rmCharComp ch xs = [x | x <- xs, x /= ch]

-- (11 simboluri)
prop_rmChar :: Char -> String -> Bool
prop_rmChar ch xs = (rmCharComp ch xs) == (rmChar ch xs)



-- 3. Comprehensions vs. map & filter
-- a. 
upperChars :: String -> String
upperChars s = [toUpper c | c <- s, isAlpha c]

-- (7 simboluri)
upperChars' :: String -> String
upperChars' str = map toUpper (filter isAlpha str)

prop_upperChars :: String -> Bool
prop_upperChars s = upperChars s == upperChars' s

-- b. 
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

-- (13 simboluri)
largeDoubles' :: [Int] -> [Int]
largeDoubles' numbers = map (\x -> 2 * x) (filter (\x -> x > 3) numbers)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- c.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

-- (11 simboluri)
reverseEven' :: [String] -> [String]
reverseEven' strs = map reverse (filter (\x -> even (length x)) strs)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
productRec :: [Int] -> Int
productRec []     = 1
productRec (x:xs) = x * productRec xs

-- (7 simboluri)
productFold :: [Int] -> Int
productFold = foldr (*) 1 

prop_product :: [Int] -> Bool
prop_product xs = productRec xs == productFold xs

-- b.  (16 simboluri)
andRec :: [Bool] -> Bool
andRec []     = True
andRec (False:xs) = False
andRec (True:xs) = andRec(xs)

-- (7 simboluri)
andFold :: [Bool] -> Bool
andFold = foldr (&&) True 

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs 

-- c.  (17 simboluri)
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec(xs)

-- (8 simboluri)
concatFold :: [[a]] -> [a]
concatFold = foldr (++) []

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- d.  (17 simboluri)
rmCharsRec :: String -> String -> String
rmCharsRec [] ch2 = ch2
rmCharsRec (ch:ch1) ch2 = rmCharsRec ch1 (rmChar ch ch2) 

-- (6 simboluri)
rmCharsFold :: String -> String -> String
rmCharsFold ch1 ch2 = foldr rmChar ch2 ch1

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str



type Matrix = [[Int]]


-- 5
-- a. (10 simboluri)
uniform :: [Int] -> Bool

uniform [] = True
--uniform (xs:[]) = True
--uniform (xs:xss) = if ( (xss!!0) == xs) then uniform xss else False 
uniform (xs:xss) = all (\x -> x == xs) xss

-- b. (   simboluri)
valid :: Matrix -> Bool
valid matrix = if (length matrix == 0 || length (head matrix) == 0) 
      then False else uniform (map (\x -> length x) matrix)

-- 6.

zipwith :: (Int -> Int-> Int) -> [Int] -> [Int] -> [Int]
zipwith f a b = [f x y | (x,y) <- (zip a b)]


zipwith2 :: (Int -> Int-> Int) -> [Int] -> [Int] -> [Int]
zipwith2 f a b = map (uncurry f) (zip a b)

-- 7.  (22 simboluri + 19 simboluri)  cu tot cu tratarea erorilor
plusM :: Matrix -> Matrix -> Matrix
plusM mat1 mat2 = if (valid mat1 == False) || (valid mat2 == False) || (length mat1 /= length mat2) || (length (mat1!!0) /= length (mat2!!0))
                then error "Not valid matrix" 
                else [zipWith (+) x y | (x,y) <- (zip mat1 mat2)] 

-- 8. (23 simboluri + 15 simboluri)  cu tot cu tratarea erorilor  

timesM :: Matrix -> Matrix -> Matrix
timesM mat1 mat2 = if (valid mat1 == False) || (valid mat2 == False) || (length (mat1!!0) /= length mat2)
         then error "Not valid input"
         else [timesM2 vec1 mat2 | vec1 <- mat1] 

timesM2 :: [Int] -> Matrix -> [Int]
timesM2 vec1 mat2 = [ dot vec1 vec2 | vec2 <- (transpose mat2)] 

dot :: [Int] -> [Int] -> Int
dot v1 v2 = sum [x1 * y1 | (x1,y1) <- (zip v1 v2)]


--9 bonus
isSquare :: Matrix -> Bool
isSquare matrix = (valid matrix) && (length matrix == length (head matrix) )

determ :: Matrix -> Int
determ matrix
       | isSquare matrix = resolveDet (appended matrix 0) 0
       | otherwise         = error "matricea nu e patratica"

appended :: Matrix -> Int -> Matrix
appended mat poz = if (poz == (length(mat !! 0) - 1)) then mat else appended (mat ++ [(mat!!poz)]) (poz + 1)

resolveDet :: Matrix -> Int -> Int
resolveDet mat inceput = if (inceput == (length (mat !! 0))) then 0 else ((resolve1 mat inceput 0) - (resolve2 mat inceput (length (mat!!0) - 1) ) + resolveDet mat (inceput + 1))

resolve1 :: Matrix -> Int -> Int -> Int
resolve1 mat poz1 poz2 =  if (poz2 == (length (mat !! 0)) ) then 1 else mat!!poz1!!poz2 * (resolve1 mat (poz1 + 1) (poz2 + 1) ) 

resolve2 :: Matrix -> Int -> Int -> Int
resolve2 mat poz1 poz2 = if (poz2 == -1) then 1 else mat!!poz1!!poz2 * (resolve2 mat (poz1 + 1) (poz2 - 1) ) 

--9 bonus (corect)

rmc :: [Int] -> Int -> Int -> [Int]
rmc [] nr nr2 = []
rmc (x:xs) nr nr2 = if (nr == nr2) then (rmc xs nr (nr2 + 1))  else x:(rmc xs nr (nr2 + 1)) 

rmcol :: Matrix ->Int -> Matrix
rmcol [] nr = []
rmcol (list:mat) nr =  (rmc list nr 0) : (rmcol mat)

determinant :: Matrix -> Int
determinant matrix
       | isSquare matrix = sum [elem * (determinant (rmcol poz (drop 1 matrix)))| (elem,poz) <- zip(matrix!!0,[0..((length matrix!!0) - 1)] ]
       | otherwise         = error "matricea nu e patratica"