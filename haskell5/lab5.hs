-- Informatics 1 - Functional Programming 
-- Tutorial 5
--
-- Due: the tutorial of week 7 (5/6 November)


import Control.Monad( liftM, liftM2 )
import Data.List( nub )
import Test.QuickCheck( quickCheck, 
                        Arbitrary( arbitrary ),
                        oneof, elements, sized  )

-- Warmup exercises

-- The datatype 'Fruit'
data Fruit = Apple String Bool
           | Orange String Int

-- Some example Fruit
apple, apple', orange :: Fruit
apple  = Apple "Granny Smith" False -- a Granny Smith apple with no worm
apple' = Apple "Braeburn" True     -- a Braeburn apple with a worm
orange = Orange "Sanguinello" 10    -- a Sanguinello with 10 segments

fruits :: [Fruit]
fruits = [Orange "Seville" 12,
          Apple "Granny Smith" False,
          Apple "Braeburn" True,
          Orange "Sanguinello" 10]

-- This allows us to print out Fruit in the same way we print out a list, an Int or a Bool.
instance Show Fruit where
  show (Apple variety hasWorm)   = "Apple("  ++ variety ++ "," ++ show hasWorm  ++ ")"
  show (Orange variety segments) = "Orange(" ++ variety ++ "," ++ show segments ++ ")"

tr :: [String]
tr = ["Tarocco", "Moro", "Sanguinello"]

-- exemple:
-- 1. Folosind case
-- worms x = case x of Apple variety True -> ... 
--                           _ -> ...
-- 2. folosind pattern matching
-- worms (Apple variety True) = case x of Apple variety True -> ... 
-- worms (Orange variety segments) = case x of Apple variety True -> ... 

-- tips: puteti folosi o astfel de declaratie de argument daca aveti nevoie
-- de ambele: fruit ca data structure si campurile lui separate

-- worms fruit@(Orange variety segments) = ... 

-- 1.
isBloodOrange :: Fruit -> Bool
isBloodOrange(Apple _ _) = False
isBloodOrange(Orange "Sanguinello" _) = True
isBloodOrange(Orange "Moro" _) = True
isBloodOrange(Orange "Tarocco" _) = True
isBloodOrange(Orange _ _) = False

worms2 :: Fruit -> Bool
worms2 (Apple _ True) = True
worms2 (Orange _ _ ) = False

-- 2.
bloodOrangeSegments :: [Fruit] -> Int
bloodOrangeSegments fruits= length [f | f <- fruits , isBloodOrange f == True]

-- 3.
worms :: [Fruit] -> Int
worms fruits = length [f | f <-fruits , worms2 f]

-- Implementing propositional logic in Haskell
-- The datatype 'Prop'

type Name = String
data Prop = Var Name
          | F
          | T
          | Not Prop
          | Prop :|: Prop
          | Prop :&: Prop
          | Prop :<->: Prop
          | Prop :->: Prop
          deriving (Eq, Ord)

type Names = [Name]
type Env = [(Name, Bool)]


-- Functions for handling Props

-- turns a Prop into a string approximating mathematical notation
showProp :: Prop -> String
showProp (Var x)        =  x
showProp (F)            =  "F"
showProp (T)            =  "T"
showProp (Not p)        =  "(~" ++ showProp p ++ ")"
showProp (p :|: q)      =  "(" ++ showProp p ++ "|" ++ showProp q ++ ")"
showProp (p :&: q)      =  "(" ++ showProp p ++ "&" ++ showProp q ++ ")"
showProp (p :<->: q)    =  "(" ++ showProp p ++ "<->" ++ showProp q ++ ")"
showProp (p :->: q)     =  "(" ++ showProp p ++ "->" ++ showProp q ++ ")"

-- evaluates a proposition in a given environment
eval :: Env -> Prop -> Bool
eval e (Var x)        =  lookUp x e
eval e (F)            =  False
eval e (T)            =  True
eval e (Not p)        =  not (eval e p)
eval e (p :|: q)      =  eval e p || eval e q
eval e (p :&: q)      =  eval e p && eval e q
eval e (p :->: q)     =  if( (eval e p == True) && (eval e q == False)) then False else True
eval e (p :<->: q)    =  ( eval e (p :->: q)) && ( eval e (q :->: p))

-- retrieves the names of variables from a proposition - 
--  NOTE: variable names in the result must be unique
names :: Prop -> Names
names (Var x)        =  [x]
names (F)            =  []
names (T)            =  []
names (Not p)        =  names p
names (p :|: q)      =  nub (names p ++ names q)
names (p :&: q)      =  nub (names p ++ names q)
names (p :->: q)     =  nub (names p ++ names q)
names (p :<->: q)    =  nub (names p ++ names q)

-- creates all possible truth assignments for a set of variables
envs :: Names -> [Env]
envs []      =  [[]]
envs (x:xs)  =  [ (x,False):e | e <- envs xs ] ++
                [ (x,True ):e | e <- envs xs ]

-- checks whether a proposition is satisfiable
satisfiable :: Prop -> Bool
satisfiable p  =  or [ eval e p | e <- envs (names p) ]


-- Exercises ------------------------------------------------------------

-- 4.
p1 = ((Var "P" :|: Var "Q") :&: (Var "P" :&: Var "Q"))
p2 = ((Var "P" :|: Var "Q") :&: (Not (Var "P") :&: Not(Var "Q"))) 
p3 = ((Var "P" :&: (Var "Q" :|: Var "R")) :&: ((Not (Var "P") :|: Not (Var "Q")) :&: (Not (Var "P") :|: Not(Var "R"))))


-- 5. 
tautology :: Prop -> Bool
tautology prop = if ((length [ False | f <- (envs $ names prop) , (eval f prop) == False]) > 0) then False else True 

prop_taut1 :: Prop -> Bool
prop_taut1 prop = (tautology prop) || (satisfiable prop)

prop_taut2 :: Prop -> Bool
prop_taut2 prop = (not $ tautology prop) || (not $ satisfiable prop) 


-- 6.
p4 = ((Var "P" :->: Var "Q") :&: (Var "P" :<->: Var "Q"))
p5 = ((Var "P" :->: Var "Q") :&: (Var "P" :&: Not(Var "Q")))
p6 = ((Var "P" :<->: Var "Q") :&: ((Var "P") :&: (Not(Var "Q")) :|: (Not(Var "P") :&: (Var "Q")))) 


-- 7.
equivalent :: Prop -> Prop -> Bool
equivalent prop1 prop2 = if ( (length [ False | (x,y) <- zip (envs $ names prop1) (envs $ names prop2), (eval x prop1) /= (eval y prop2)]) == 0) then True else False

equivalent' :: Prop -> Prop -> Bool
equivalent' = undefined

prop_equivalent :: Prop -> Prop -> Bool
prop_equivalent = undefined


-- 8.
subformulas :: Prop -> [Prop]
subformulas (Var x)        =  [Var x]
subformulas (F)            =  []
subformulas (T)            =  []
subformulas (Not p)        =  (Not p):(subformulas p)
subformulas (p :|: q)      =  (p :|: q):((subformulas p) ++ (subformulas q))
subformulas (p :&: q)      =  (p :&: q):((subformulas p) ++ (subformulas q))
subformulas (p :->: q)     =  (p :->: q):((subformulas p) ++ (subformulas q))
subformulas (p :<->: q)    =  (p :<->: q):((subformulas p) ++ (subformulas q))

-- Optional Material

-- 9.
-- check for negation normal form
isNNF :: Prop -> Bool
isNNF prop = if ((length [x | x <- subformulas prop, isNNF2 x == False]) > 0) then False else True

isNNF2 :: Prop -> Bool
isNNF2 (Not prop) = isNNF3(prop)
isNNF2 (p :->: q) = False
isNNF2 (p :<->: q) = False
isNNF2 (_) = True

isNNF3 :: Prop -> Bool
isNNF3 (Var x) = True
isNNF3 (_) = False      
-- 10.
-- convert to negation normal form

toNNF2 :: Prop -> Prop
toNNF2 prop = if (isNNF prop == True) then prop else (toNNF prop)

toNNF :: Prop -> Prop
toNNF (Not (p :|: q)) = ((Not (toNNF2 p)) :|: (Not (toNNF2 q)))
toNNF (Not (p :&: q)) = ((Not (toNNF2 p)) :&: (Not (toNNF2 q)))
toNNF (p :->: q) = ((Not (toNNF2 p)) :|: (toNNF2 q))
toNNF (p :<->: q) = ((toNNF2 p) :->: (toNNF2 q)) :&: ((toNNF2 q) :->: (toNNF2 p))
toNNF (Not (Not p)) = toNNF2 p
toNNF (p :&: q) = (toNNF2 p) :&: (toNNF2 q)
toNNF (p :|: q) = (toNNF2 p) :|: (toNNF2 q)
toNNF (T) = T
toNNF (F) = F

-- check if result of toNNF is in neg. normal form
prop_NNF1 :: Prop -> Bool
prop_NNF1 p  =  isNNF (toNNF2 p)

-- check if result of toNNF is equivalent to its input
prop_NNF2 :: Prop -> Bool
prop_NNF2 p  =  equivalent p (toNNF2 p)


-- 11.
-- check whether a formula is in conj. normal form
isCNF :: Prop -> [Prop]
isCNF (Var x)        =  [Var x]
isCNF (F)            =  [F]
isCNF (T)            =  [T]
isCNF (Not p)        =  (Not p):(isCNF p)
isCNF (p :|: q)      =  (p :|: q):((isCNF p) ++ (isCNF q))
isCNF (p :&: q)      =  (p :&: q):((isCNF p) ++ (isCNF q))
isCNF (p :->: q)     =  (p :->: q):((isCNF p) ++ (isCNF q))
isCNF (p :<->: q)    =  (p :<->: q):((isCNF p) ++ (isCNF q))


-- 13.
-- transform a list of lists into a (CNF) formula
listsToCNF :: [[Prop]] -> Prop
listsToCNF = undefined


-- 14.
-- transform a CNF formula into a list of lists
listsFromCNF :: Prop -> [[Prop]]
listsFromCNF = undefined


-- 15.
-- transform an arbitrary formula into a list of lists
toCNFList :: Prop -> [[Prop]]
toCNFList = undefined



-- convert to conjunctive normal form
toCNF :: Prop -> Prop
toCNF p  =  listsToCNF (toCNFList p)

-- check if result of toCNF is equivalent to its input
prop_CNF :: Prop -> Bool
prop_CNF p  =  equivalent p (toCNF p)




-- For QuickCheck --------------------------------------------------------

instance Show Prop where
    show  =  showProp

instance Arbitrary Prop where
    arbitrary  =  sized prop
        where
          prop n | n <= 0     =  atom
                 | otherwise  =  oneof [ atom
                                       , liftM Not subform
                                       , liftM2 (:|:) subform subform
                                       , liftM2 (:&:) subform subform
                                     --  , liftM2 (:->:) subform subform
                                     --  , liftM2 (:<->:) subform' subform'
                                       ]
                 where
                   atom = oneof [liftM Var (elements ["P", "Q", "R", "S"]),
                                   elements [F,T]]
                   subform  =  prop (n `div` 2)
                   subform' =  prop (n `div` 4)


-- For Drawing Tables ----------------------------------------------------

-- centre a string in a field of a given width
centre :: Int -> String -> String
centre w s  =  replicate h ' ' ++ s ++ replicate (w-n-h) ' '
            where
            n = length s
            h = (w - n) `div` 2

-- make a string of dashes as long as the given string
dash :: String -> String
dash s  =  replicate (length s) '-'

-- convert boolean to T or F
fort :: Bool -> String
fort False  =  "F"
fort True   =  "T"

-- print a table with columns neatly centred
-- assumes that strings in first row are longer than any others
showTable :: [[String]] -> IO ()
showTable tab  =  putStrLn (
  unlines [ unwords (zipWith centre widths row) | row <- tab ] )
    where
      widths  = map length (head tab)

table p = tables [p]

tables :: [Prop] -> IO ()
tables ps  =
  let xs = nub (concatMap names ps) in
    showTable (
      [ xs            ++ ["|"] ++ [showProp p | p <- ps]           ] ++
      [ dashvars xs   ++ ["|"] ++ [dash (showProp p) | p <- ps ]   ] ++
      [ evalvars e xs ++ ["|"] ++ [fort (eval e p) | p <- ps ] | e <- envs xs]
    )
    where  dashvars xs        =  [ dash x | x <- xs ]
           evalvars e xs      =  [ fort (eval e (Var x)) | x <- xs ]

-- print a truth table, including columns for subformulas
fullTable :: Prop -> IO ()
fullTable = tables . filter nontrivial . subformulas
    where nontrivial :: Prop -> Bool
          nontrivial (Var _) = False
          nontrivial T       = False
          nontrivial F       = False
          nontrivial _       = True


-- Auxiliary functions

lookUp :: Eq a => a -> [(a,b)] -> b
lookUp z xys  =  the [ y | (x,y) <- xys, x == z ]
    where the [x]  =  x
          the _    =  error "eval: lookUp: variable missing or not unique"