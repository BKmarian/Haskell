{-
Definim un tip de date pentru a reprezenta o rețea de mulțimi de puncte în plan:
-}

type Punct = (Int,Int)
data Multime = X 
             | Y 
             | DX Int Multime 
             | DY Int Multime 
             | U Multime Multime
{-
Planul este centrat în punctul (0,0).  Prima coordonată a unui punct (coordonata x) reprezintă distanța pe orizontală de la origine iar a doua (coordonata y) reprezintă distanța pe verticală.  Prin convenție, coordonatele x cresc spre dreapta, în timp ce coordonatele y cresc în sus.  

Constructorul X reprezintă mulțimea punctelor de pe axa x, adică punctele care au coordonata y zero:
... (-2,0) (-1,0) (0,0) (1,0) (2,0) ...


Constructorul Y reprezintă mulțimea punctelor de pe axa y, adică punctele care au coordonata x zero:
... (0,-2) (0,-1) (0,0) (0,1) (0,2) ...

Constructorul DX dx p, unde dx este un întreg și p e o mulțime de puncte, reprezintă punctele p deplasate la dreapta cu dx. De exemplu, DX 2 Y are ca rezultat axa y deplasată cu două unități la dreapta:
... (2,-2) (2,-1) (2,0) (2,1) (2,2) ...

Observație 1:  DX dx X și X denotă aceeași mulțime de puncte, deoarece prin deplasarea axei x pe orizontală se obține tot axa x.

Observație 2: Un punct (x,y) aparține lui DX dx p dacă și numai dacă punctul (x-dx,y) aparține lui p.

Constructorul DY dy p, unde dy este un întreg și p e o mulțime de puncte, reprezintă punctele p deplasate în sus cu dx. De exemplu, DY 3 X are ca rezultat axa X deplasată cu două unități în sus:
... (-2,3) (-1,3) (0,3) (1,3) (2,3) ...

Observație 1:  DY dy Y și Y denotă aceeași mulțime de puncte, deoarece prin deplasarea axei y pe verticală se obține tot axa y.

Observație 2: Un punct (x,y) aparține lui DY dy p dacă și numai dacă punctul (x,y-dy) aparține lui p.

Constructorul U p q, unde p și q sunt mulțimi de puncte, reprezintă reuniunea punctelor din p și q.  De exemplu,
U (U X Y) (U (DY 3 X) (DX 2 Y))
reprezintă mulțimea de puncte de forma

... (-2,0) (-1,0) (0,0) (1,0) (2,0) ...
... (0,-2) (0,-1) (0,0) (0,1) (0,2) ...

... (-2,3) (-1,3) (0,3) (1,3) (2,3) ...
... (2,-2) (2,-1) (2,0) (2,1) (2,2) ...

Cerințe:
 Scrieți o funcție
-} 

apartine :: Punct -> Multime -> Bool
apartine punct X = if (snd punct == 0) then True else False
apartine punct Y = if (fst punct == 0) then True else False
apartine punct (DX i multime) = (apartine ( (fst punct) - i , (snd punct)) multime)
apartine punct (DY i multime) = (apartine ( (fst punct) , (snd punct) - i) multime)
apartine punct (U m1 m2) = (apartine ((fst punct) , (snd punct)) m1) || (apartine ((fst punct) , (snd punct)) m2)

{-
 care determină dacă un punct aparține unei mulțimi de puncte date.  De exemplu:

  apartine (3,0) X == True
  apartine (0,1) Y == True
  apartine (3,3) (DY 3 X) == True
  apartine (2,1) (DX 2 Y) == True
  apartine (3,0) (U X Y) == True
  apartine (0,1) (U X Y) == True
  apartine (3,3) (U (DY 3 X) (DX 2 Y)) == True
  apartine (2,1) (U (DY 3 X) (DX 2 Y)) == True
  apartine (3,0) (U (U X Y) (U (DX 2 Y) (DY 3 X))) == True
  apartine (0,1) (U (U X Y) (U (DX 2 Y) (DY 3 X))) == True
  apartine (3,3) (U (U X Y) (U (DX 2 Y) (DY 3 X))) == True
  apartine (2,1) (U (U X Y) (U (DX 2 Y) (DY 3 X))) == True
  apartine (1,1) X == False
  apartine (1,1) Y == False
  apartine (1,1) (DY 3 X) == False
  apartine (1,1) (DX 2 Y) == False
  apartine (1,1) (U X Y) == False
  apartine (1,1) (U X Y) == False
  apartine (1,1) (U (DY 3 X) (DX 2 Y)) == False
  apartine (1,1) (U (DY 3 X) (DX 2 Y)) == False
  apartine (1,1) (U (U X Y) (U (DX 2 Y) (DY 3 X))) == False

Scrieți o funcție 
-}

nrAxe :: Multime -> Int
nrAxe X = 1
nrAxe Y = 1
nrAxe (DX i multime) = nrAxe multime
nrAxe (DY i multime) = nrAxe multime
nrAxe (U m1 m2) = (nrAxe m1) + (nrAxe m2)

{-
 care numără de câte ori apare X sau Y în descrierea unei mulțimi de puncte. Fiecare axă trebuie numărată câte o dată pentru fiecare apariție a sa. De exemplu:

  nrAxe X == 1
  nrAxe Y == 1
  nrAxe (U X Y) == 2
  nrAxe (U (DY 3 X) (DX 2 Y)) == 2
  nrAxe (U (U X Y) (U (DX 2 Y) (DY 3 X))) == 4
  nrAxe (U (U X Y) X) == 3 



Se consideră următorul tip algebric de date:
-}

data Expr = Const Int
          | Neg Expr
          | Expr :+: Expr
          | Expr :*: Expr
  deriving (Eq, Show)

data Op = NEG | PLUS | TIMES
  deriving (Eq, Show)

data Atom = AConst Int | AOp Op
  deriving (Eq, Show)

type Polish = [Atom]


{-
Cerințe:
Să se scrie o funcție 
-}

fp :: Expr -> Polish
fp (Const i) = [AConst i]
fp (Neg ex1) = [AOp NEG] ++ (fp ex1)
fp (ex1 :+: ex2) = [AOp PLUS] ++ (fp ex1) ++ (fp ex2) 
fp (ex1 :*: ex2) = [AOp TIMES] ++ (fp ex1) ++ (fp ex2) 

{-
care asociază unei expresii aritmetice date scrierea ei în forma poloneză: o listă de Atomi, obținută prin parcurgerea în preordine a arborelui asociat expresiei (operațiile precedând reprezentărilor operanzilor).
Exemple:
* forma poloneză a expresiei 5 * 3 este * 5 3 
  fp (Const 5 :*: Const 3) == [AOp TIMES, AConst 5, AConst 3]

* forma poloneză a expresiei −(7 * 3) este  - * 7 3
  fp (Neg (Const 7 :*: Const 3)) == [AOp NEG, AOp TIMES, AConst 7, AConst 3]

* forma poloneză a expresiei (5 + −3) * 17 este * + 5 - 3 17
  fp ((Const 5 :+: Neg (Const 3)) :*: Const 17) == [AOp TIMES, AOp PLUS, AConst 5, AOp NEG, AConst 3, AConst 17]

* forma poloneză a expresiei (15 + (7 * (2 + 1))) * 3 este * + 15 * 7 + 2 1 3
  fp ((Const 15 :+: (Const 7 :*: (Const 2 :+: Const 1))) :*: Const 3) == [AOp TIMES, AOp PLUS, AConst 15, AOp TIMES, AConst 7, AOp PLUS, AConst 2, AConst 1, AConst 3]

Definiți o funcție 
-}

rfp :: Polish -> Maybe Expr
rfp [] = Nothing
rfp exp = Just (snd (rfp2 exp))

rfp2 :: Polish -> (Polish , Expr)
rfp2 (AOp PLUS:lista) = (fst (rfp2 x) , (y:+:(snd (rfp2 x)))) where (x,y) = (rfp2 lista)
rfp2 (AOp TIMES:lista) = (fst (rfp2 x) , (y:*:(snd (rfp2 x)))) where (x,y) = (rfp2 lista)
rfp2  (AOp NEG:lista)   = (x,Neg y) where (x,y) = (rfp2 lista)
rfp2  ((AConst i):lista)  = (lista,Const i)
{-
astfel încât rfp . fp = Just . id

Introducem un tip de date ce reprezinta o colectie de puncte (o tabela). 
-}

type Point =(Int, Int)

data Points = Rectangle Point Point
            | Union Points Points
            | Difference Points Points 

{-
Tabela incepe cu punctul (0,0) stanga jos. 
Constructorul Rectangle selecteaza toate punctele dintr-o forma rectangulara.
De pilda, Rectangle (0,0) (2,1) da colturile din stanga jos si dreapta sus ale unui dreptunghi si include punctele (0,0) ; (1,0) ;(2,0) ; (0,1) ; (1,1) ; (2,1)

Union combina doua colectii de puncte iar Difference contine acele puncte care sunt in prima colectie dar nu sunt in a doua. 			  

Scrieti o functie perimeter care calculeaza perimetrul celui mai mic dreptunghi care cuprinde complet o colectie de puncte.
-}

--perimeter :: Points -> Int
--perimeter (Rectangle p1 p2) = ( (x - xx) * 2  + (y - yy) * 2) where x = (fst p1) y = (snd p1) xx = (fst p2) yy = (snd p2)
--perimeter (Union p1 p2) = Rectangle ((minim x z),(minim y t)) ((minim z zz),(minim t tt)) where (x,y) = p1 (z,t) = p2

--toRectangle :: Points -> ((Int,Int),(Int,Int))
--toRectangle (Rectangle p1 p2) = (Rectangle p1 p2)
--toRectangle (Union p1 p2) = Rectangle ((minim x z),(minim y t)) ((maxim z zz),(maxim t tt)) 
--                            where ((x,y) , (z,t)) = (toRectangle p1) ((xx,yy) , (zz,tt)) = (toRectangle p2)
--toRectangle (Difference p1 p2) = Rectangle ((minim x z),(minim y t)) ((maxim z zz),(maxim t tt)) 
--                            where ((x,y) , (z,t)) = (toRectangle p1) ((xx,yy) , (zz,tt)) = (toRectangle p2)


minim :: Int -> Int -> Int
minim x y = if(x < y) then x else y

maxim :: Int -> Int -> Int
maxim x y = if(x > y) then x else y

{-
-- ajutatoare 
dreptunghi Union p1 p2 = Rectangle ((minx,miny),(minx,miny))  where minx = (minim (fst p1) (fst p2))   miny = (minim (snd p1) (snd p2)) maxx = (maxim (fst p1) (fst p2))   maxy = (maxim (snd p1) (snd p2))

 Scrieti o functie distance care calculeaza distanta dintre doua colectii de puncte ca reprezentand distanta intre coltul dreapta-sus al dreptunghiului minimal care cuprinde prima colectie si coltul stanga-jos al dreptunghiului minimal care cuprinde cea de-a doua colectie.
-}

distance :: Points -> Points -> Float
distance = undefined

{-
Introducem un tip de date ce reprezinta o expresie booleana formată din literali / variabile (Lit), negație (Not), conjuncție (And), disjuncție (Or) și implicație (:->:), in care conjuncțiile si disjuncțiile au un număr arbitrar de termeni.-} 

data Exp = Lit String
         | Not Exp
         | And [Exp]
         | Or [Exp]
         | Exp :->: Exp
  deriving (Show)

{-
Un atom este fie un literal (Lit), fie negația unui literal. O expresie este în formă normală disjunctivă dacă este o disjuncție de conjuncții de atomi.

Să se scrie o funcție care dată fiind o expresie are ca rezultat forma normală disjunctivă a acelei expresii.
-}

isAnd :: Exp -> Bool
isAnd (And _) = True 
isAnd _ = False

removeAnd :: Exp -> [Exp]
removeAnd (And e1) = e1 

isOr :: Exp -> Bool
isOr (Or _) = True
isOr _ = False

removeOr :: Exp -> [Exp]
removeOr (Or e1) = e1 

dnf :: Exp -> Exp
dnf (Lit s) = Lit s
dnf (Not (Not e1)) = dnf e1
dnf (Not (And lista)) = Or (map (\x -> Not (dnf x)) lista)
dnf (Not (Or lista)) = And (map (\x -> Not (dnf x)) lista)
dnf (Not x) = (dnf (Not x))
dnf (e1:->:e2) = Or [Not (dnf e1), (dnf e2)]
dnf (And lista) = And ( (map dnf (concat (map removeAnd (filter isAnd lista)))) ++ (map dnf (filter (\x -> (isAnd x) == False) lista)) )
dnf (Or lista) = Or ( (map dnf (concat (map removeOr (filter isOr lista)))) ++ (map dnf (filter (\x -> (isOr x) == False) lista)) )

{-
   dnf ((Lit "a" :->: Lit "b") :->: Lit "c")
     = Or [And [Lit "a",Not (Lit "b")],And [Lit "c"]]
   dnf (Lit "a" :->: (Lit "b" :->: Lit "c"))
     = Or [And [Not (Lit "b")],And [Lit "c"],And [Not (Lit "a")]]

• a → b ≡ ¬a ∨ b
• ¬¬a = a
• ¬ /\ (a 1 , . . . , a n ) = \/(¬a 1 , . . . , ¬a n )
• ¬ \/ (a 1 , . . . , a n ) = /\(¬a 1 , . . . , ¬a n )
• /\ (a 1 , . . . , a i , /\(b 1 , . . . , b m ), a i+1 , . . . , a n ) = /\ (a 1 , . . . , a n , b 1 , . . . , b m )
• \/ (a 1 , . . . , a i , \/ (b 1 , . . . , b m ), a i+1 , . . . , a n ) = \/ (a 1 , . . . , a n , b 1 , . . . , b m )
• /\ (a 1 , . . . , a i , \/ (b 1 , . . . , b m ), a i+1 , . . . , a n ) = \/ (/\ (b 1 , a 1 , . . . , a n ), . . . , /\ (b m , . . . , a n ))

-}
