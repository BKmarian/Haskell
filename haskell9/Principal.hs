module Principal where

import Arbore

main :: IO [Int]
main = do
		x <- getLine
		return $ parcurgere (ini (map read (words x) :: [Int]))

   
