{- LISTA 2-}

{- 1 -}
concatena [] a = a
concatena (b:bs) a = b:concatena bs a

{- 2 replica-}
replica 0 a = []
replica n a =
	do
	a:replica (n-1) a

{- 3 elemento -}
elemento n [] = False
elemento n (x:xs) = if n == x then True 
		else elemento n xs

{- 4 isSorted -}
isSorted [] = False
isSorted [x] = True
isSorted (x:xs) = if x <= menor xs then True 
		else False

menor [y] = y
menor (y:ys) = if y <= menor ys then y 
		else 0

{- 5 palindromo Não Recursivo -}
palindromo [] = True
palindromo [_] = True
palindromo (x:xs) = if reverse (x:xs) == (x:xs) then True 
		else False

{- 6 rotEsq -}
rotEsq 0 x = x 
rotEsq 1 (x:xs) = xs ++ [x] 
rotEsq a (x:xs) = rotEsq (a-1) xs ++ [x]

{- 7 rotDir -}
rotDir 0 x = x
rotDir n (x:xs) =
	do  
 	rotDir (n-1) (take (length (x:xs)) (last(x:xs):(x:xs)))

{- 8 uniao -}
pertence a [] = False
pertence a (x:xs) = if (a == x) then True
		else pertence a xs

uniao [] n = n
uniao (x:xs) n = if pertence x n then uniao xs n
		else x: uniao xs n

{- 9 somaDigitos -}
somaDigitos a = if a < 10 then a 
		else (mod a 10) + somaDigitos (div a 10)

{- 10 binario-}
bin a	| (a == 0) = [0]
	| (a == 1) = [1]
	| (mod a 2 == 1) = bin (div a 2) ++ [1]
	| otherwise = bin (div a 2) ++ [0]

{- 11 Partir -}
partir a []=[]
partir a xs= [take a xs] ++ partir a (drop a xs)

{- 12 insert -}
insert x [] = [x]
insert x (y:ys) = if x <= y then x:y:ys 
		else y:insert x ys

ordena [] = []
ordena [x] = [x]
ordena (x : xs) = insert x (ordena xs)

{- ---------- -}
insertSort1::Ord a => [a] -> [a]
insertSort1 [] = []
insertSort1 [x] = [x]
insertSort1 (x:xs) = insert x (insertSort1 xs)


{- 13 -}
minimo [x] = x 
minimo (x:xs) = if x > head xs then minimo xs 
	else minimo (x:(tail xs)) 

remove a [] = [] 
remove a (x:xs) = if (a==x) then xs 
	else x: remove a xs

{- 14 -}

{- 15 sumq-}
sumq 0 = 0
sumq n =
	do
	quadrado n + sumq(n-1)

quadrado a = a*a  

{- 16 Funcao Tamanho -}
lengthr, lengthl :: [Int] -> Int
lengthr = foldr (\x y -> 1 + y) 0
lengthl = foldl (\x y -> x + 1) 0

{- 17 concat -}
concatenaFold xs ys = foldr (:) ys xs

{- 18 inverte -}
inverte :: [a] -> [a]
inverte = foldr (\x xs -> xs ++ [x]) [] 
	
{- 19 ineverte2 -}
inverte2 = foldl (\xs x-> [x] ++ xs) [] 


{- 21 elemAny -}
elemAny a [] = False
elemAny a (x:xs) = any (a==) (x:xs)

{- 23 mapFoldr -}
mapFoldr f []       = []
mapFoldr f (x:xs)   = foldr (\y ys -> (f y):ys) [] xs

{- 24 filtro-}
filtro f = foldr (\x -> if (f x) then (x:) else id) []

{- 25 maior -}
maior xs = foldr1 (\x y -> max x y) xs

