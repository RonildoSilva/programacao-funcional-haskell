--1
--a
map f [] = [] --i
map f [x] = f [x]
--P.B:
	map f [] = map f [] ++ map f []
		  [] = [] ++ []
--HI. 
	map f (xs ++ ys) = map f xs ++ map f ys 
--P.I:
	map f (x:xs ++ y:ys) 					= map f x:xs ++ map f y:ys
	f x ++ map f(xs) ++ f y ++ map f (ys)	= f x ++ map f (xs) ++ f y : map f (ys)

----b
	reverse [] = [] --ii
	reverse [x] = [x]
	reverse (x:xs) = reverse xs : [x]
----P.B:
	map f (reverse []) = reverse (map f []) 
			 map f []  = reverse [] --por i e ii
				   []  = []
----H.I:
	map f (reverse xs) = reverse (map f xs)
----P.I:
	map f (reverse x:xs) 					= reverse (map f x:xs)
	map f (reverse xs : [x])				= reverse (f x ++ map f xs)
	map f (reverse xs) ++ map f x			= reverse f x ++ reverse map f xs
	reverse(map f xs) ++ f x				= f x ++ reverse map f xs (HI)

--c
lenght [] = 0
lenght [x] = 1
lenght (x:xs) = 1 + lenght xs
--P.B:
lenght(map f []) = lenght []
			0	 =	0
--H.I:
lenght(map f xs) = lenght xs
--PI.: 
lenght(map f x:xs) 			   = lenght x:xs
lenght(f x : map f xs)		   = 1 + lenght xs
lenght(f x): length (map f xs) =
		1  + lenght xs 		   = (HI) 

--d
tail [] = []
tail [x] = []
tail (x:xs) = xs 
--P.I:
map f (tail []) = tail(map f [])
map f ([])		= tail([])
			 []	= []
--H.I:
map f (tail xs) = tail (map f xs)
--P.I:
map f (tail x:xs) 		= tail(map f x:xs)
map f xs	 			= tail(f x : map f xs)
						= map f xs
--e
concat [] = []
concat [[x]] = [x]
concat [[x],[y]] = [x,y]

--P.B:
map f concat[]  = concat(map f (map f []))
map f 		[]	= concat(map f [])
			[]	= []
--H.I: 
map f concat xs = concat(map f (map f xs))
--P.I: 
map f concat x:xs = concat(map f (map f x:xs))
map f [x:xs]	  = concat(map f (f x : map f xs))          
f x : map f xs	  = concat(f (f x) : map f (map f xs))
				  = concat f (f x) : concat (map f (map f xs))
				  = f (f x) : map f concat xs (HI)
----f
--P.B: foldr(:) [] [] = []
--H.I:foldr(:) [] xs = xs
--P.I:
--foldr(:) [] x:xs = x:xs
----------------------------------------------------------------------
--2
take 0 xs = []	--take 0
take n [] | n > 0 = [] --take 1
take n (x:xs) | n > 0 = x : take (n-1) xs --take2

drop 0 xs = xs --drop 0
drop n [] | n > 0 = [] --drop 1
drop n (x:xs) | n > 0 = drop (n-1) xs -- drop 2
--a
--P.B: 
take m (drop n []) = drop n (take (m+n) [])
take m (    []   ) = drop n (	  []	  ) 
		[]		   = 			[]
--H.I: 
take m (drop n xs) = drop n (take (m+n) xs)
--P.I:
take m (drop n x:xs  )= drop n (take (m+n) x:xs) --take 2
take m (drop (n-1) xs)= drop n (x :take (m+n-1) xs) --drop 2
					  = drop (n-1) (take (m+n-1) xs)

--b
--PB
take n [] ++ drop n [] = []
--HI.
take n xs ++ drop n xs = xs
--PI.
take n x:xs ++ drop n x:xs 			= x:xs
x : take (n-1) xs ++ drop (n-1) xs	= x:xs
x : xs								= x:xs (HI)

--c
--PB
drop m (drop n []) = drop(m+n) []
		drop m []  = []
			   []  = []
--Hi.
drop m (drop n xs) = drop(m+n) xs
--PI
drop m (drop n x:xs  ) = drop(m+n) x:xs
drop m (drop (n-1) xs) = drop(m+n -1) xs
					   = drop m (drop (n-1) xs)

--d
--PB
take m (take n []) = take (min n m) []
take m [] 		   = []
				[] = [] 
--HI
take m (take n xs) = take (min n m) xs
--P.I
take m (take n x:xs) 			= take (min n m) x:xs
take m (x: take (n-1) xs)		= x: take ((min n m)-1) xs
x: (take (m-1) (take (n-1) xs)) = x: take m-1 (take n-1 xs) (HI) 
--e
--PB
take m (drop n []) = drop n take (m+n) []
		take m []  = drop n []
			   []  = []
--HI
take m (drop n xs) = drop n take (m+n) xs
--P.I
take m (drop n x:xs) 	= drop n take (m+n) x:xs
take m (drop(n-1) xs)	= drop n x : take (n+m)-1 xs
drop n-1 take m+n-1 xs 	= drop n-1 take n+m-1 xs --1(HI)
--f
drop(m take n xs) = take (n-m) (drop m xs)

--3
--a
splitAt n xs = (take n xs, drop n xs)
sugestão where (ys, zs) = splitAt n xs
--b
splitAt n cs = (take n xs, drop n xs)


--9
sum [] = 0
sum(x:xs) = x + sum xs
--a
sumAcc xs z = sum xs + z
xs = []
--P.B
sumAcc [] z = sum [] + z 
			= 0 + z
sumAcc [] z	= z
sumAcc(y:ys) z  = sum(y:ys) + z
				= y + sum ys + z
				= sum ys + (z + y)
				= sumAcc ys(z + y)

--11
map f [] = []
map f (x:xs) = f x : map f xs
foldr g z [] = []
foldr g z (x:xs) = g x (foldr g z xs)

map f xs = foldr g z xs
--P.B.
map f [] = foldr gz []
	  [] = z
map f (x:xs) = foldr gz (x:xs)
f x : map f xs = g x (foldr g z xs)
f x : a  	   = g x a
g x a 		   = g x a	

--16
data Arv a = Vazia | No a (Arv a) (Arv a)

a1 = No 1 (Vazia)(Vazia)
a2 = No 3 (No 1 Vazia Vazia) (No 4 Vazia Vazia)
a3 = No 10 (No 5 (No 3 Vazia Vazia)(No 7 Vazia Vazia)) (No 20 (No 15 Vazia Vazia)(No 30 Vazia Vazia)) 

--a
tamanho:: Arv a -> Int
tamanho Vazia = 0 -- (1)
tamanho (No x esq dir) = 1 + tamanho(esq) + tamanho (dir) -- (2)
--b
mapArv::(a -> b) -> Arv a -> Arv b
mapArv f Vazia = Vazia -- (3)
mapArv f (No x esq dir) = (No (f x) (mapArv f esq) (mapArv f dir)) -- (4)
--c 
--Usando indução sobre árvores, prove
--PB: Seja uma árvore Vazia
--		tamanho (mapArv f Vazia) = tamanho Vazia
--(3) 	tamanho (Vazia) 		 = (1) 0
--(1) 			0 				 =     0

--H.I: 	tamanho(mapArv f (No x esq dir)) 	= tamanho (No x esq dir) -- (5)
--P.I: 	tamanho(mapArv f (No x esq dir))+ 1 = tamanho (No x esq dir) + 1
--		tamanho(No x esq dir) + 1 			= tamanho(esq) + tamanho(dir) +2
--		1 + tamanho(esq)+tamanho(dir) + 1 	= tamanho(esq) + tamanho(dir) +2
--		tamanho(esq) + tamanho(dir) +2 		= tamanho(esq) + tamanho(dir) +2

--import Test.QuickCheck
--quickCheck(\t f -> tamanho(t) == tamanho(mapArv f t))

--18
--a
soma:: Arv Int -> Int
soma Vazia = 0
soma (No x esq dir) = x + soma(esq)+ soma(dir)
--b
foldArv:: b -> (a -> b -> b -> b) -> Arv a -> b
foldArv z f Vazia  = z 
foldArv z f (No x esq dir) = f x (foldArv z f esq) (foldArv z f dir)
--c usando indução mostre que 
--soma t = foldArv 0 (\ x y z -> x + y + z)
--PB Vazia