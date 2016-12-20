-- Informatics 1 - Functional Programming
-- Tutorial 7
--
-- Week 9 - Due: 19/20 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (x :#: y) = split x ++ split y
split Sit = []
split a = [a]

-- 1b. join
join :: [Command] -> Command
join [] = Sit
join (x:xs) |xs==[]   = x
            | otherwise = x :#: join xs


-- 1c  equivalent
split :: Command -> [Command]
split  = (x :#: y) = split x ++ split y
split Sit = []
split a = [a]

equivalent :: [Command] -> [Command]
equivalent (x:xs) = if x == Sit then equivalent xs else x :#: equivalent xs



-- 1d. testing join and split
prop_split_join = undefined

prop_split = undefined


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy 0 xs = Sit
copy nr x = replicate nr x

-- sau
copy 0 xs = Sit
copy x xs = xs :#: copy (x-1) xs

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon x = copy 5 (Go 50.0 :#: Turn 72.0)

-- 2c. polygon
polygon :: Distance -> Int -> Command
polygon x y = copy y (Go x :#: Turn (360 / fromIntegral y)) 



-- Exercise 3
-- spiral
spiral :: Distance -> Int -> Distance -> Angle -> Command
spiral _ 0 _ _ = Sit
spiral side n step angle = Go side :#: Turn angle :#: spiral (side + step)(n-1)step angle


-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise x | test (split x) == 1  = optimise $ join $ opt $ split x 
           | otherwise = x


test :: [Command] -> Int

test (Turn x :Turn y:xs)        =  1
test (Go x : Go y : xs)         =  1
test (Turn x:xs) | x==0         =  1 
                 | otherwise    =  test xs 
test (Go x : xs) | x==0         =  1 
                 | otherwise    =  test xs 
test (Sit : xs) = 1
test [] = 0


opt :: [Command] -> [Command]

opt (Turn x :Turn y:xs)        =  opt $ Turn(x+y):xs 
opt (Go x : Go y : xs)         =  opt $ Go(x+y):xs
opt (Turn x:xs) | x==0         =  opt xs 
                 | otherwise    =  Turn x : opt xs 
opt (Go x : xs) | x==0         =  opt xs 
                 | otherwise    =  Go x : opt xs 
opt (Sit : xs) = opt xs
opt [] = []

--optimise (Go 10 :#: Sit :#: Go 20 :#: Turn 35 :#: Go 0 :#: Turn 15 :#: Turn (-50)) 


-- L-Systems

-- 5. arrowhead
arrowhead :: Int -> Command
arrowhead = undefined

-- 6. snowflake
snowflake :: Int -> Command
snowflake = undefined

-- 7. hilbert
hilbert :: Int -> Command
hilbert = undefined

main :: IO ()
main = display (Go 30 :#: Turn 120 :#: Go 30 :#: Turn 120 :#: Go 30)
