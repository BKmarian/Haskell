module Audit where
import Data.Char
import Test.QuickCheck
import Control.Monad (liftM, ap)
import Control.Applicative

type Audit a = (a, String)

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
bind f (gRes, gStr) = (fRes, gStr ++ fStr)
  where (fRes, fStr) = f gRes

add10 :: Float -> (Float, String)
add10 x = (x + 10, "add10 was called")

divideIn2 :: Float -> (Float, String)
divideIn2 x = (x / 2, "divideIn2 was called")

ex1test = bind add10 . divideIn2 $ 5


-- Using # instead of * for composition to avoid ambiguities
(#) :: (b -> Audit c) -> (a -> Audit b) -> (a -> Audit c)
f # g = bind f . g

ex1test2 = add10 # divideIn2 $ 5



-- Exercise 2
unit :: a -> Audit a
unit x = (x, "")

-- lift --- lifting functions
lift :: (a -> b) -> (a -> Audit b)
lift f = unit . f


add7 :: Float -> Float
add7 x = x + 7

ex2test = lift add7 $ 1



-- Test that (for a given value x) lift g # lift f = lift (g.f)
-- For simplicity we restrict to Float functions as in the tutorial
check_lift :: (Float -> Float) -> (Float -> Float) -> Float -> Bool
check_lift f g x = comp1 x == comp2 x
  where comp1 = lift f # lift g
        comp2 = lift (f . g)

test_lift :: IO ()
test_lift = quickCheck $ check_lift (+2) (*3)

-- Exercise Ten (a): Rewrite the module to make Audit instance of
-- the Monad typeclass

-- newtype e un fel de prescurtare a lui data
-- folosim newtype pt ca nu putem face instanta de Monad, Applicative pe o fctie
newtype Debuggable a = Debuggable (a, String) deriving Show

unitDeb :: a -> Debuggable a
unitDeb x = Debuggable (x, "")

bindDeb :: Debuggable a -> (a -> Debuggable b) -> Debuggable b
bindDeb (Debuggable (gRes, gStr)) f = Debuggable (fRes, gStr ++ fStr)
  where Debuggable (fRes, fStr) = f gRes

instance Functor Debuggable where
  fmap = liftM

instance Applicative Debuggable where
  pure = unitDeb
  -- pure = return
  -- (<*>) = ap

instance Monad Debuggable where
  return = unitDeb
  (>>=) = bindDeb

testDebuggableMonad =
  do
    let x = 10
    y <- Debuggable (x + 1, "inc")
    Debuggable (y / 2, "half")

testDebuggableBind =
  Debuggable (10, "init, ") >>=
  \x -> Debuggable (x+1, "incr, ") >>=
  \y -> Debuggable (y/2, "hal")

