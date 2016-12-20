module Arbore (Tree,
               adauga,
               cauta,
               ini,
               parcurgere)
where


data Tree a = Leaf
            | Node a (Tree a) (Tree a)
            deriving Show

-- tree for testing
root :: Tree Int
root = (Node 7 (Node 3 (Node 1 Leaf Leaf) (Node 5 Leaf Leaf)) (Node 10 Leaf Leaf))
--        7
--      /   \
--     3     10
--    / \    
--   1   5

adauga :: Ord a => a -> Tree a -> Tree a
adauga x Leaf = Node x Leaf Leaf 
adauga x arb@(Node a left right) | x == a = (Node a left right)
								 | x < a = (Node a (adauga x left) right)
								 | otherwise = (Node a left (adauga x right))


cauta :: Ord a => a -> Tree a -> Maybe a
cauta x Leaf = Nothing
cauta x (Node a left right) | x == a = Just x
							| x < a = (cauta x left) 
							| otherwise = (cauta x right)



ini :: Ord a => [a] -> Tree a
--ini xs = foldr (adauga) Leaf xs
ini [] = Leaf
ini (x:xs) = adauga x (ini xs)  

-- parcurgere (ini [1,5,2,8,10,3,11,6,7]) == [1,2,3,5,6,7,8,10,11]
parcurgere :: Tree a -> [a]
parcurgere Leaf = []
parcurgere (Node a left right) = parcurgere left ++ [a] ++ parcurgere right 
