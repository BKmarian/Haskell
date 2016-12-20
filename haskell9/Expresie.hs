module Expresie (Expr,
               expresie,
               calculeaza)
where

data Expr a = Var String | Var2 a | (Expr a) :+: (Expr a) | (Expr a) :*: (Expr a) deriving (Read,Show)

expresie :: Expr Int
expresie = (((Var "a") :*: (Var2 3)) :+: ((Var2 3) :*: (Var "b")))


extrage :: Num a => Expr a -> [String]
extrage (Var e) = [e]
extrage (Var2 i) = []
extrage (exp1 :+: exp2) = (extrage exp1) ++ (extrage exp2)
extrage (exp1 :*: exp2) = (extrage exp1) ++ (extrage exp2)


findInt :: Num a => String -> [(a,String)] -> a
findInt str lista = if ((snd $ head lista) == str) then (fst $ head lista) else (findInt str (tail lista)) 

calculeaza :: Num a => Expr a -> [(a,String)] -> a
calculeaza (Var e) lista = findInt e lista
calculeaza (Var2 e) lista = e
calculeaza (exp1 :+: exp2) lista = (calculeaza exp1 lista) + (calculeaza exp2 lista)
calculeaza (exp1 :*: exp2) lista = (calculeaza exp1 lista) * (calculeaza exp2 lista)