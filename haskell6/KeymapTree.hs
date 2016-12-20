-- INF 1 Functional Programming
-- 
-- Indexed data represented as a tree


module KeymapTree ( Keymap,
                    size, depth,
                    get, set, 
                    toList, fromList                 
                  )

where

-- Modules for testing

import Test.QuickCheck
import Control.Monad
import Data.List

-- The data type

data Keymap k a = Leaf
                | Node k a (Keymap k a) (Keymap k a)

-- A test tree

testTree :: Keymap Int Int
testTree = Node 2 20 (Node 1 10 Leaf Leaf)
                     (Node 3 30 Leaf 
                               (Node 4 40 Leaf Leaf ))

-- Exercise 6

size :: Ord k => Keymap k a -> Int
size Leaf = 0
size (Node _ _ left right) = 1 + size left + size right

depth :: Ord k => Keymap k a -> Int
depth (Node _ _ Leaf Leaf) = 1
depth (Node _ _ left Leaf) = (depth left) + 1
depth (Node _ _ Leaf right) = (depth right) + 1
depth (Node _ _ left right) = (max (depth left) (depth right)) + 1
-- Exercise 7

toList :: Ord k => Keymap k a -> [(k,a)]
toList (Node x y Leaf Leaf) = [(x,y)]
toList (Node x y Leaf right)= [(x,y)] ++ (toList right)
toList (Node x y left Leaf) = (toList left) ++ [(x,y)]
toList (Node x y left right)= (toList left) ++ [(x,y)] ++ (toList right)

-- Exercise 8

set :: Ord k => k -> a -> Keymap k a -> Keymap k a
set key value = f
    where
      f Leaf = Node key value Leaf Leaf
      f (Node k v left right) | key == k  = Node k value left right
                              | key < k  = Node k v (set key value left) right
                              | otherwise = Node k v left (set key value right)

-- Exercise 9

get :: Ord k => k -> Keymap k a -> Maybe a
get key nod = f nod
    where
      f Leaf = Nothing
      f (Node k v left right) | key == k  = Just v
                              | key < k  = f left
                              | otherwise = f right

prop_set_get :: Int -> Int -> Bool
prop_set_get k v = get k (set k v testTree) == Just v

-- Exercise 10

fromList :: Ord k => [(k,a)] -> Keymap k a
fromList [] = Leaf
fromList lista = foldr (\(x,y) -> set x y) Leaf lista


prop_toList_fromList :: [Int] -> [Int] -> Bool
prop_toList_fromList xs ys = sort (toList (fromList zs)) == sort zs
    where
      zs = zip (nub xs) ys

prop_toList_fromList_sorted :: [Int] -> [Int] -> Bool
prop_toList_fromList_sorted xs ys = toList (fromList zs) == sort zs
    where
      zs = zip (nub xs) ys

-- Optional Material -----------------------------------

-- Exercise 12

filterLT :: Ord k => k -> Keymap k a -> Keymap k a
filterLT el keymap = fromList (filter (\(x,y) -> x < el) (toList keymap))

filterGT :: Ord k => k -> Keymap k a -> Keymap k a
filterGT el keymap = fromList (filter (\(x,y) -> x > el) (toList keymap))

-- Exercise 13

merge :: Ord k => Keymap k a -> Keymap k a -> Keymap k a
merge k1 k2 = fromList ((toList k1) ++ (toList k2))

-- Exercise 14

del :: Ord k => k -> Keymap k a -> Keymap k a
del el keyMap = merge (filterLT el keyMap) (filterGT el keyMap)

-- Exercise 15

select :: Ord k => (a -> Bool) -> Keymap k a -> Keymap k a
select f keymap = fromList (filter (\(x,y) -> (f y) == True) (toList keymap)) 

-- Instances for QuickCheck -----------------------------
instance (Ord k, Show k, Show a) => Show (Keymap k a) where
    show = show . toList

instance (Ord k, Arbitrary k, Arbitrary a) => Arbitrary (Keymap k a) where
    arbitrary = liftM fromList $ liftM2 zip (liftM nub arbitrary) arbitrary
