module Citire where

import Expresie

main :: IO Int
main = do
    x <- getLine
    let v1 = map toString (evens x)
    let v2 = (map read (map toString (odds x))) :: [Int]
    return $ calculeaza expresie (zip v2 v1)

--createList :: [String] -> [(Int,String)]
--createList (line : lista) = ( read x :: Int,y):(createList lista) where x = line!!0 
--                                                                        y = [line!!2]  

toString :: Char -> String
toString x = [x]

odds :: String -> [Char]
odds [] = []
odds [x] = []
odds (e1:e2:xs) = e2 : odds xs


evens :: String -> [Char]
evens [] = []
evens [x] = [x]
evens (e1:e2:xs) = e1 : odds xs