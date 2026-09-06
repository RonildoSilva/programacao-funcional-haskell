import Test.QuickCheck

{-16-}
data Arv a = Vazia|No a (Arv a) (Arv a)

arv1 = Vazia
arv2 = (No 1 Vazia Vazia)
arv3 = (No 3 Vazia Vazia)
arv4 = (No 5 arv1 arv2)
arv5 = (No 7 Vazia Vazia)
arv6 = (No 9 arv4 arv5)

--tamanho::Arv a->Int
tamanho Vazia = 0
tamanho (No a esq dir) = tamanho esq + tamanho dir + 1

--mapArv::(a->b)->Arv a->Arv b
mapArv f Vazia = Vazia
mapArv f (No a esq dir) = (No (f a) (mapArv f esq) (mapArv f dir))

{-18-}
somaArv Vazia = 0
somaArv (No a esq dir) = a + somaArv(esq) + somaArv(dir)

--foldArv:: b -> (a -> b -> b -> b) -> Arv a -> b
foldArv z f Vazia  = z 
foldArv z f (No x esq dir) = f x (foldArv z f esq) (foldArv z f dir)
--foldArv 2 (\x y z -> x+y+z ) arv5

{-
BASE Vazia

soma Vazia = foldArv 0 f Vazia

HIP
soma esq = foldArv 0 f esq
soma dir = foldArv 0 f dir

PI
soma (No x esq dir)
x + soma esq + soma dir
x + foldArv 0 f + foldArv
-}

{-9-}
sum [] = 0
sum (x:xs) = x + sum xs

--sumAcc xs z = sum xs + z

--xs = []
--sumAcc [] z = sum[] + z
--   		  = 0 + z
--            = z

--xs = (y:ys)
--sumAcc (y:ys) x = sum(y:ys) + z
--                = Y + sum ys + z
--                = sum ys + (z+y)
--                = sumAcc ys (x+y)

sumAcc [] z = z
sumAcc (y:ys) z = sumAcc ys (z+y)

{-11-}

{-
--DEFINICAO

map f xs = foldr g z xs
map f [] = []
map f (x:xs) = f x:map f xs

foldr g z [] = z
foldr g z (x:xs) = g x (foldr g z xs)
-}

--map f [] = foldr g z []
--      [] = z
--       z = []

{-
map f (x:xs) = foldr g z (x:xs)
f x:(map f xs) = g x (foldr g z xs)
f x:a = g x a
g x a = (f x:a)
\x a-> f x : a
-}

{-13-}
filter p [] = []
filter p (x:xs)
	| p x = x : filter p xs
	| otherwise = filter p xs

foldr f z [] = x
foldr f z (x:xs) = f x (foldr f z xs)

-----------------------------------------------------------

sumAcc::[Int] -> Int -> Int
sumAcc [] !z = z
sumAcc (y:ys) !z = sumAcc ys (z+y) 

map f xs = foldr g z xs
map f [] = []
map f (x:xs) = f x:map f xs

foldr g z [] = z
foldr g z (x:xs) = g x (foldr g z xs)

map f [] = foldr g z []
	  [] = z

map f (x:xs) = foldr g z (x:xs)
f x:map f xs = g x (foldr g z xs)
f x:a = g x a
g x a = (f x:a)

map f xs = foldr (\x a-> f x:a) [] xs

filter p [] = []
filter p (x:xs) | p x = x:filter p xs
				| otherwise = filter p xs

foldr f z [] = z
foldr f z (x:xs) = f x (foldr f z xs)

filter p xs = foldr g z xs	

Caso Base: foldr f z [] = filter p []
					z = [] 	
HI: foldr g z xs = filter p xs	
PI: foldr g z (x:xs) = filter p (x:xs)
	g x (foldr g z xs) = if p x then x:filter p xs
							else filter p xs
	g x ys			   = if p x then x:ys							
							else ys
filter p xs = foldr (\x ys -> if p x then x:ys else ys) [] xs							


fst (x,y) = x (1)
snd (x,y) = y (2)

map f [] = [] (3)
map f (x:xs) = f x:map f xs (4)

sum [] = 0 (5)
sum (x:xs) = x + sum xs (6)

rld [] = [] (7)
rld (x:xs) = (replicate (fst x) snd(x))++rld xs (8)

length (xs++ys) = length xs + length ys (9)
length (replicate n x) = n | n>0 (10)

length [] = 0 (11)
length (x:xs) = 1 + length xs (12)

length (rld ps) = sum (map fst ps) (HI)

Caso Base: length (rld []) = sum (map fst []) (7, 11, 3)
					[] = sum([]) (5)
					[] = 0
HI: length (rld ps) = sum (map fst ps)
PI: length (rld p:ps) = sum(map fst p:ps) (8, 4)
	length ((replicate (fst p) snd(p)++rld ps)) = sum(fst p:map f ps) (9)					
	length(replicate fst(p) snd(p)) + length rld ps = sum(fst p:map fst ps) (10, HI)
	fst(p) + sum(map fst ps) = sum(fst p:map fst ps) (6)
	fst(p) + sum(map fst ps) = fst p + sum(map fst ps)


-----------------------------------------------------------
