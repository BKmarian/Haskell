module MyRandom where

import System.Random
import Test.QuickCheck

import Control.Monad (liftM, ap)
import Control.Applicative


type MyRandom a = StdGen -> (a, StdGen)

-- Standard Random Number Generator seeded with 0
zeroSeed :: StdGen
zeroSeed = mkStdGen 0

-- System.Random.random is of type MyRandom a
--    - if a implements the Random type class
-- For example, try
-- :t random::MyRandom Int


-- Random value in interval [0,n]
uniformNat :: Int -> MyRandom Int
uniformNat n = uint
  where
    uint gen = let (m, gen') = random gen
               in (m `mod` n, gen')

-- fst $ uniformNat 100 zeroSeed   ==  41

-- Exercise Seven:  implement bind
bind :: (a -> MyRandom b) -> (MyRandom a -> MyRandom b)
bind f g seed = f g' seed'
  where (g', seed') = g seed

-- Using # instead of * for composition to avoid ambiguities
(#) :: (b -> MyRandom c) -> (a -> MyRandom b) -> (a -> MyRandom c)
g' # f' = bind g' . f'

-- random of random :-)
doubleRandom :: Int -> MyRandom Int
doubleRandom = uniformNat # uniformNat

-- Exercise Eight: implement unit -- leave the generator unodified
unit :: a -> MyRandom a
-- unit x g = (x, g) -- same
unit = (,)

addRand :: Float -> MyRandom Float
addRand x seed = (x + val, newSeed)
  where (val, newSeed) = random seed

-- addRandComp :: Float -> MyRandom Float
-- addRandComp x = i
  -- where (i, r) = bind addRand . addRand $ x


-- lift --- lifting functions
lift :: (a -> b) -> (a -> MyRandom b)
lift f = unit . f

-- Random value in a given interval
--    - uniformFromTo m n = m + uniformNat (n-m)
uniformFromTo :: Int -> Int -> MyRandom Int
uniformFromTo m n = lift (m+) # uniformNat $ (n-m)

-- TOASK how can i reuse the seed to print a new random number every time?
printableRng :: Int -> Int -> Int
printableRng m n = i
  where (i, r) = uniformFromTo m n zeroSeed

-- Exercise Nine:  Test that (for a given value x)
-- (a) f # unit = unit # f = f
-- (b) lift g # lift f = lift (g.f)
check_unit1, check_unit2 :: (a -> MyRandom b) -> a -> Bool
check_unit1 f x = undefined
check_unit2 f x = undefined

test_unit1, test_unit2 :: IO ()
test_unit1 = quickCheck $ check_unit1 uniformNat
test_unit2 = quickCheck $ check_unit2 uniformNat

check_lift :: (Float -> Float) -> (Float -> Float) -> Float -> Bool
check_lift f g x = undefined

test_lift :: IO ()
test_lift = quickCheck $ check_lift (+2) (*3)

-- Exercise Ten(c): Rewrite the module to make MyRandom instance of
-- the Monad typeclass

newtype Randomised a = Randomised (StdGen -> (a, StdGen)) --deriving Show

instance Functor Randomised where
  fmap = liftM

instance Applicative Randomised where
  pure = return
  (<*>) = ap

instance Monad Randomised where
  return x = Randomised (\s -> (x, s)) -- seed doesn't change
  Randomised ra >>= f = Randomised (\s -> let (a, s') = ra s
                                              rb = f a
                                          in rb s')

-- Exercise Eleven(c):  Write the uniformFromTo function in do notation

