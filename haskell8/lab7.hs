-- Informatics 1 - Functional Programming 
-- Tutorial 7
--
-- Week 9 - Due: 19/20 Nov.


import LSystem
import Test.QuickCheck

-- Exercise 1

-- 1a. split
split :: Command -> [Command]
split (Go d) = [Go d]
split (Turn a) = [Turn a]
split (Sit) = []
split(c1:#:c2) = (split c1) ++ (split c2)

-- 1b. join
join :: [Command] -> Command
join (command:[]) = command:#:Sit
join (command:lista) = command:#:(join lista)
join [] = Sit

join2 :: [Command] -> Command
join2 (comm:[]) = comm
join2 ((Go 0):lista) = join lista
join2 ((Go d):lista) = (Go d):#:(join lista)
join2 ((Turn 0):lista) = join lista
join2 ((Turn e):lista) = (Turn e):#:(join lista)
-- 1c  equivalent

equivalent = split ((Go 3 :#: Turn 4) :#: (Sit :#: Go 7)) == split (((Sit :#: Go 3) :#: Turn 4) :#: Go 7)

-- 1d. testing join and split

prop_split_join :: Command -> Bool
prop_split_join c = (join $ split c) == c

prop_split :: Command -> Bool
prop_split c = if ([c] == (split c)) then True else False


-- Exercise 2
-- 2a. copy
copy :: Int -> Command -> Command
copy i c = (join (replicate i c))

-- 2b. pentagon
pentagon :: Distance -> Command
pentagon dist = pentagon2 dist 5 72

pentagon2 :: Distance -> Float -> Float -> Command
pentagon2 dist 1 lung = (Go dist):#:(Turn lung)
pentagon2 dist i lung = (Go dist):#:(Turn lung):#:(pentagon2 dist (i - 1) lung)

-- 2c. polygon

polygon :: Distance -> Float -> Command
polygon dist nr = polygon2 dist nr (360/nr)

polygon2 :: Distance -> Float -> Float -> Command
polygon2 dist 1 lung = (Go dist):#:(Turn lung)
polygon2 dist i lung = (Go dist):#:(Turn lung):#:(polygon2 dist (i - 1) lung)


-- Exercise 3
-- spiral
spiral :: Distance -> Float -> Distance -> Angle -> Command
spiral side n step angle = spiral2 side n n step angle

spiral2 :: Distance -> Float -> Float -> Distance -> Angle -> Command
spiral2 side n 1 step angle = (Go side):#: (Turn angle)
spiral2 side n i step angle = (Go side):#: (Turn angle):#:(spiral2 (side + step) n (i - 1) step angle)
-- Exercise 4
-- optimise
optimise :: Command -> Command
optimise (Sit :#: command) = optimise command
optimise (Go 0 :#: command) = optimise (Sit :#: command)
optimise (Go d :#: Go e :#: command) = optimise (Go (d + e) :#: command)
optimise (Turn 0:#:command) = optimise (Sit :#: command)
optimise (Turn a :#: Turn b :#: command) = optimise (Turn (a + b) :#: command)
optimise (Turn a :#: Turn b) = Turn (a + b)
optimise (Go a :#: Go b) = Go (a + b)
optimise (Go d:#:Sit:#:command) = optimise (Go d :#: command)
optimise command = command


-- 6. snowflake
snowflake :: Int -> Command
snowflake x = f x :#: p :#: p :#: f x :#: p :#: p :#: f x :#: p :#: p
  where
  f 0      = GrabPen red :#: Go 10
  f x      = f (x-1) :#: n :#: f (x - 1) :#: p :#: p :#: f (x-1) :#: n :#: f (x-1)
  n        = Turn 60
  p        = Turn (-60)

-- 7. hilbert
hilbert :: Int -> Command
hilbert x = l x
          where
          l 0      = GrabPen red :#: Go 10
          l x      = n :#: r (x - 1) :#: p :#: l (x - 1) :#: l (x - 1) :#: p :#: r (x - 1) :#: n
          r 0      = GrabPen blue :#: Go 10
          r x      = p :#: l (x - 1) :#: n :#: r (x - 1) :#: r (x - 1) :#: n :#: l (x - 1) :#: p
          n        = Turn 90
          p        = Turn (-90)
          
main :: IO ()
main = display (pentagon 50)

-- L-Systems

-- 5. arrowhead

arrowhead :: Int -> Command
arrowhead x = f x
  where
  f 0      = GrabPen red :#: Go 10
  f x      = g (x-1) 
             :#: n :#: f (x-1)
             :#: n :#: g (x-1)
  g 0      = GrabPen blue :#: Go 10
  g x      = f (x-1) :#: p :#: g (x-1) :#: p :#: f (x - 1)
  n        = Turn 60
  p        = Turn (-60)