{-
Zero + m = m (1)
Succ n + m = Succ(m+n) 2

-}

length[]=0
length (x:xs) = 1 + lenght xs

replicate 0 x = []
replicate n x | n > 0 = x: replicate (n-1) x

length (replicate n x) = n

{-
BASE n  = 0
	length (replicate 0 x) = 0
	lenght [] = 0

HI
	length (replicate x) = n (4)

PI
	length (x : replicate n x) (2)
	1 + length (replicate n x) (HI)

-}

[] ++ ys = ys
(x:xs) ++ ys = x:(xs++ys)

-- xs ++ [] = xs ???

--Indução sob listas:
--P([]) -> P(x:xs)
--P(xs) -> P(x:xs)

{-
BASE []++[]=[]
     []=[]

HI
	xs ++ [] = xs

PI
	(x:xs)++[] = (x:xs) (2)
	x:(xs++[]) (HI)
	x:xs

-}

reverse [] = []
reverse (x:xs) = reverse xs++[x]


reverse (reverse xs) = xs
asdf = a+sdf -> 
reverse(reverse (a + sdf)) -> 
reverse(reverse a + reverse (sdf)) - >
reverse(a + reverse (sdf)) - >
reverse (ŕeverse(sdf)+a) = 
a + reverse(sdf)

{-

BASE [] = []

HI
	[x] = [x]
	(x:xs) = [x]++xs

PI
	(x:xs) = [x]++xs
	reverse(reverse ([x] ++ xs))
	reverse(a ++ reverse xs)
	reverse(reverse xs ++ [x])
	a ++ reverse(reverse xs)

	CASO EM ABERTO HUEHUEHUEHUHUE

--------------------------------------------

lema1: reverse(xs++ys) = reverse ys ++ reverse xs
lema2: (xs++ys)++zs = xs++(ys++zs)

BASE reverse (reverse[]) = [] (1)
     reverse ([]) = []
     [] =  []

HI
	reverse (reverse xs) = xs

PI
	reverse(reverse (x:xs)) = (x:xs) (2)
	reverse(reverse (reverse xs++[x]) (lema1)
	reverse(reverse [x] ++ reverse(reverse xs)) (HI)
		[x] ++ xs
		(x:xs)

-}



length [] = 0
length (x:xs) = 1+length xs

sum [] = 0
sum (x:xs) = x+ sum xs

map f [] = []
map f (x:xs) = fx : map f xs

--1)
sum (map (+1) xs) = length xs + sum xs

{-
BASE
	sum (map f []) = 0
	sum [] = 0
	
	length [] = 0
	
	sum (map f []) = length []

HI
	sum (map (+1) (x:xs)) = lenght (x:xs) + sum (x:xs)
	sum (map (+1) (xs)) = lenght xs + sum (xs)

PI
	sum (+1) x + sum (map (+1) xs) = lenght x + sum x + lenght xs + sum xs
	(x + 1) + sum (map (+1) xs) = (1 + x) + lenght xs + sum xs

-}

--2)

lema: map f (xs++ys) = map f xs ++ map f ys

map f (reverse xs) = reverse (map f xs)

{-

BASE

	map f (reverse []) = 0
	map f ([]) = 0

	reverse (map f []) = 0
	reverse (0) = 0
	
HI
	map f (reverse (x:xs)) = reverse (map f (x:xs))

PI
	map f (reverse [x]) ++ map f (reverse xs) = reverse (map f [x])) ++ reverse (map f xs)
	map f [x] ++ map f (reverse xs) = map f [x] ++ reverse (map f xs)

	map f (reverse xs) = reverse (map f xs)

-}

data Arv a = Folha | No a (Arv a) (Arv a)

leaves:: retorna o número de folha
nos:: retorna o número de nos internos

leaves(t) = nodes(t)+1
p(folha)

p(esq), p(dir) -> p(No x esq dir)
----------------------------------
p(t)

{-

*No interno é um nó que possui no mínino um filho

PROVA
	1) SUB-CASO (Nenhum filho)

	*leaves(Folha) = nodes(Folha)+1
	             1 =           0 +1

	2) SUB-CASO (Apenas um filho)

	 leaves(esq, Folha) = nodes(esq, Folha) + 1
	 leaves(esq) + leaves(Folha) = nodes(esq) + nodes(Folha) + 1
         leaves(esq) + 1 = nodes(esq) + 1

	a) leaves(esq) = nodes(esq)

	a.1) leaves(esq) = leaves(Folha,dir)
			leaves(Folha,dir) = nodes(Folha,dir) + 1
			leaves(Folha) + leaves(dir) = nodes(Folha) + nodes(dir) + 1
			1 + leaves(dir) = nodes(dir) + 1
		        leaves(dir) = nodes(dir)
			
        a.2) leaves(dir) = leaves(esq,Folha)
			leaves(esq,Folha) = nodes(esq,Folha) + 1
			leaves(esq) + leaves(Folha) = nodes(esq)+ nodes(Folha) + 1
			leaves(esq) + 1 = nodes(esq) + 1
			leaves(esq) = nodes(esq)

	3) SUB-CASO (Dois filhos)
	 leaves(esq, dir) = nodes(esq, dir) + 1		
	 leaves(dir) + leaves(Folha) + leaves(esq) + leaves(Folha) = nodes(esq) + 1 + nodes(dir) + 1

-----------------------------------------------ATÉ AQUI TÁ BOM --------------------------------------------	 
HI


	leaves((No x esq Folha)) = nodes((No x esq Folha))+1
	1 + leaves(esq) + leaves(Folha) = nodes(esq) + nodes(Folha)  +  1
	1 + leaves(esq) + 1             = nodes(esq) + nodes(Folha)  +  1
	1 + 1 = nodes(Folha)

	 a
	/ 
       s  
      / \
     d   f
-}








