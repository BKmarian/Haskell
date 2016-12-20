data Eithers a b  = Lefts a | Rights b
type Lista = [Eithers Int String]

list::Lista
list = [Lefts 4,Lefts 5,Rights "salam",Rights "wing",Lefts 1]

sumInts :: Lista -> Int
sumInts mylista = sum [x | Lefts x <- mylista]