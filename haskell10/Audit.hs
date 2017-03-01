module Audit where
import Data.Char
import Test.QuickCheck

type Audit a = (a,String)

-- Given a function, debug produces a function logging information 
-- about the computed value for each argument
debug :: (Show a, Show b) =>  (a -> b) -> (a -> Audit b)
debug f x = (y, show x ++ " |-> " ++ show y ++ "; ")
  where
    y = f x

ord' :: Char -> Audit Int
ord' = debug ord

chr' :: Int -> Audit Char
chr' = debug chr

-- Exercise 1
bind :: (a -> Audit b) -> (Audit a -> Audit b)
bind f (x,d) = (s,d++t) where (s,t) = f x

-- Using # instead of * for composition to avoid ambiguities
(#) :: (b -> Audit c) -> (a -> Audit b) -> (a -> Audit c)
g' # f' = bind g' . f'

-- Exercise 2
unit :: a -> Audit a
unit x = (x,"")

-- lift --- lifting functions
lift :: (a -> b) -> (a -> Audit b)
lift f = unit . f

-- Test that (for a given value x) lift g # lift f = lift (g.f) 
-- For simplicity we restrict to Float functions as in the tutorial
check_lift :: (Float -> Float) -> (Float -> Float) -> Float -> Bool
check_lift f g x = (lift g # lift f) x == lift (g . f) x

test_lift :: IO ()
test_lift = quickCheck $ check_lift (+2) (*3)

-- Exercise Ten (a): Rewrite the module to make Audit instance of 
-- the Monad typeclass

data Log a = Log { val :: a, log :: String}
instance Monad Log where  
    return x = Log a ""
    ma >>= k = Log { val = val mb , log = log ma ++ log mb } where mb = k (val ma)
    fail x = (x,"") 

logIncrement :: Int -> Log Int
logIncrement x = Log (x + 1) ("Called increment with argument" ++ show x ++ "\n")
logIncrement2 :: Int -> Log Int
logIncrement2 x = logIncrement x >>= logIncrement 