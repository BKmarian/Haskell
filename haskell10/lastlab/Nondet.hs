module Nondet where
import Data.Complex
import Test.QuickCheck
import Control.Monad (liftM, ap)
import Control.Applicative

type Nondet a = [a]

sqrts :: Floating a => a -> Nondet a
sqrts x = [y, negate y]
  where
    y = sqrt x


-- Exercise Four:  Apply the function on all input values and aggregate the results
bind :: (a -> Nondet b) -> (Nondet a -> Nondet b)
bind = concatMap -- the linter won't shut up if I don't do this simplification

oppositeToo :: Float -> [Float]
oppositeToo x = [x, -x]

plus1n2n3 :: Float -> [Float]
plus1n2n3 x = [x + 1, x + 2, x + 3]

ex4test = bind oppositeToo . plus1n2n3 $ 0


-- Using # instead of * for composition to avoid ambiguities
(#) :: (b -> Nondet c) -> (a -> Nondet b) -> (a -> Nondet c)
g' # f' = bind g' . f'


-- Exercise Five: Provide minimal context for the given value
unit :: a -> Nondet a
unit x = [x]

-- lift --- lifting functions
lift :: (a -> b) -> (a -> Nondet b)
lift f = unit . f


square :: Float -> Float
square x = x^2

ex5test = (lift square) $ 2


-- Solution to the quadratic equation a * x^2 + b * x + c = 0
-- delta = b^2 - 4*a*c
-- x = (-b +- sqrt delta) / (2*a)
solveQuadratic :: Floating a  => a -> a -> a -> Nondet a
solveQuadratic a b c = lift (/(2*a)) # lift (-b +) # sqrts $ delta
  where
    delta = b^2 - 4*a*c


-- Exercise Six:  Test that (for a given value x)
-- (a) f # unit = unit # f = f
-- (b) lift g # lift f = lift (g.f)
check_unit1, check_unit2 :: (Complex Float -> Nondet (Complex Float)) -> Complex Float -> Bool
check_unit1 f x = (f # unit $ x) == (unit # f $ x)
check_unit2 f x = (f # unit $ x) == f x

test_unit1, test_unit2 :: IO ()
test_unit1 = quickCheck $ check_unit1 sqrts
test_unit2 = quickCheck $ check_unit2 sqrts

check_lift :: (Float -> Float) -> (Float -> Float) -> Float -> Bool
check_lift f g x = comp1 x == comp2 x
  where comp1 = lift f # lift g
        comp2 = lift (f . g)
test_lift :: IO ()
test_lift = quickCheck $ check_lift (+2) (*3)


-- Exercise Ten(b): Rewrite the module to make Nondet instance of
-- the Monad typeclass

newtype Multivalued a = Multivalued [a] deriving Show

unitMulti :: a -> Multivalued a
unitMulti x = Multivalued [x]

-- In order to use concatMap, Multivalued must be Foldable
bindMulti :: Multivalued a -> (a -> Multivalued b) -> Multivalued b
-- bindMulti (Multivalued as) f = Multivalued (concatMap f as)
bindMulti = undefined

instance Functor Multivalued where
  fmap = liftM -- builtin

instance Applicative Multivalued where
  pure = return
  (<*>) = ap

instance Monad Multivalued where
  return = unitMulti
  -- (>>=) = bindMulti
  -- Multivalued as >>= f = Multivalued [ b | a <- as, let Multivalued bs = f a,  b <- bs ]
  Multivalued as >>= f = Multivalued (concat [ bs | Multivalued bs <- map f as ])

testMultiMonad =
  do
    let x = 10
    y <- Multivalued [x, x+1, x+2]
    Multivalued [y*2, y*4]

-- Exercise Eleven(b):  Write the solution to the quadratic equation in do notation

