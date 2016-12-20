module Domeniu (Dom,
               exist,
               overlap,
               normalize,
               optimize)
where

data Dom a = Null
             | Ran a a 
             | (Dom a) :|: (Dom a) 
             | (Dom a) :&: (Dom a)
             deriving (Show, Read)

-- 1.
exist :: Ord a => a -> Dom a -> Bool
exist _ Null = False
exist x (Ran a b) = a <= x && x <= b
exist x (i1 :|: i2) = exist x i1 || exist x i2
exist x (i1 :&: i2) = exist x i1 && exist x i2
--

-- 2.
-- O alta rezolvare se poate defini si in functie de exist (definit chiar mai sus)
overlap :: Ord a => Dom a -> Dom a -> Bool
overlap (Ran a b) (Ran c d) = (a <= c && c <= b) || (a <= d && d <= b) || (c <= a && a <= d) || (c <= b && b <= d)
overlap _ _ = False

-- 3.
normalize :: Ord a => Dom a -> Dom a
normalize Null = Null
normalize r@(Ran a b) = r
normalize ((x :|: y) :&: z) = normalize (x :&: z) :|: normalize (y :&: z)
normalize (x :|: y) = normalize x :|: normalize y
normalize (x :&: y) = normalize x :&: normalize y

-- (((Ran 1 2) :|: (Ran 3 4)) :&: (Ran 2 3))
-- 4.
optimize :: Ord a => Dom a -> Dom a
optimize (Null) = Null
optimize r@(Ran a b) = r
optimize (x :|: Null) = optimize x
optimize (Null :|: x) = optimize x
optimize (x :&: Null) = Null
optimize (Null :&: x) = Null

optimize r@((Ran a b) :|: (Ran c d)) 
         | overlap (Ran a b) (Ran c d) = (Ran (min a c) (max b d))
         | otherwise = r
optimize ((Ran a b) :&: (Ran c d)) 
         | overlap (Ran a b) (Ran c d) = (Ran (max a c) (min b d))
         | otherwise = Null
optimize (x :|: y) = optimize $ optimize x :|: optimize y
optimize (x :&: y) = optimize $ optimize x :&: optimize y

test :: Dom Int
test = ((((Ran 1 5) :|: (Ran 3 9)) :&: (Ran 0 7)) :|: ((Ran 8 11) :&: (Ran 9 14)))
teststr = "((((Ran 1 5) :|: (Ran 3 9)) :&: (Ran 0 7)) :|: ((Ran 8 11) :&: (Ran 9 14)))"
-- (([1,5] U [3,9]) & [0,7]) U ([8,10] & [9,14])
-- optimize test = Ran 1 7 :|: Ran 9 10
test2 :: Dom Int
test2 = ((((Ran 1 5) :|: (Ran 3 9)) :&: (Ran 0 7)) :|: (((Ran 8 11) :&: (Ran 9 14)) :|: Null))

test3 :: Dom Int
test3 = (((Ran 1 5 :&: Ran 0 7) :|: (Ran 3 9 :&: Ran 0 7)) :|: ((Ran 8 11 :&: Ran 9 14) :|: Null))
-- (([1,5] & [0,7]) | ([3,9] & [0,7])) | (([8,10] & [9,14]) | Null)
--optimize ((x :|: y) :&: z) = optimize $ optimize (x :&: z) :|: optimize (y :&: z)

--optimize r@((Ran a b) :|: (Inf c)) = if b >= c then Inf a else r
--optimize r@((Inf c) :|: (Ran a b)) = optimize ((Ran a b) :|: (Inf c))

--optimize ((Ran a b) :&: (Inf c)) = if b < c then Null else Ran (max a c) (max b c)
--optimize ((Inf c) :&: (Ran a b)) = if b < c then Null else Ran (max a c) (max b c)
