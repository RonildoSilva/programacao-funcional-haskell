{-Questão 1-}
{-
a) remove :: Eq a => a->[a] = [a]
b) partes :: [a] -> [[a]]
c) rota :: Int->[a]->[a]
d) swap :: (t,t1) -> (t1,t)
e) twice :: (t->t) -> a -> a 
-}

remover n (x:xs) = filter (/=n) (x:xs)

{-Questão 2-}
{-
a) recebe um elemento e uma lista, e remove o elemento dado da lista, 
caso ele esteja na lista.
b) recebe uma lista e retorna o conjunto das partes dessa lista.
c) recebe um inteiro e uma lista e retorna a lista rotacionada 
de acordo com o valor do elemento.
d) recebe um tupla e a retorna invertida
e)
-}
partes [] = [[]]
partes (x:xs) = [x : y | y <- partes xs ] ++ partes xs
{-Questão 3-}
{-a-}
prodMn m n = product[m..n]

{-b-}
sumQuad n = sum [n*n | n <- [0..n]]

{-c-}
interior [] = []
interior [a] = [a]
interior (a:as) = take (length (a:as)-2) $tail (a:as)

--init (drop 1 lista)

{-d-}
segmento m n (x:xs) = if m > n then [] else 
					(x:xs) !! (m-1) : segmento (m+1) (n) (x:xs)

{-Questão 4-}
{-a-}
subconjunto [] _ = True
subconjunto (x:xs) ys = elem x ys && subconjunto xs ys

--sunConj (x:xs) (y:ys) = [x | x<-(x:xs), elem x (y:ys)]
sunConj (x:xs) (y:ys) = and [elem x (y:ys) | x<-xs]

{-b-}
uniao [] n = n
uniao (x:xs) n = if elem x n then uniao xs n
		else x: uniao xs n

union xs ys = xs ++ [y | y<-ys, notElem y xs]

{-c-}
diferencia xs ys = [x | x<-xs, notElem x ys]

{-Questão 5-}
aproximaPi n = sqrt (6 * (sum $map (\x->(1/)x^2) [1..n]))


{-Questão 6-}
maiorSalto (x:xs) = abs (minimum (x:xs) - maximum (x:xs))


{-Questão 7-}
{-a-}

--elemFoldr n (x:xs) = foldr (\n xs -> n==x) False xs
elem' x xs = foldr(\y ys -> if x == y then True else ys) False xs

--foldr ( == ) False [True ,False ,False ,True ]


{-b-}
remdup xs = foldr(\x xs -> if elem x xs then xs else (x:xs)) [] xs

{-c-}
predicadoQualquer n = 2*n + 3 `mod` 3 == 0
todos (x:xs) = foldr (&&) True (map predicadoQualquer (x:xs))

todos' p xs = foldr (\x ys-> if p x && ys then True else False) True xs
map' f xs = foldr(\y ys-> f y : ys) [] xs
{-d-}
positivo n = if n >= 0 then True else False
positivos (x:xs) = foldr (&&) True (map positivo (x:xs))

{-e-}
algum (x:xs) = foldr (||) True (map predicadoQualquer (x:xs))

--algum' p xs = foldr(\y ys -> if p y || ys then True else False) False

{-Questão 8-}
prefixo (x:xs) (y:ys) = if (x:xs) == take (length(x:xs)) (y:ys) then True 
						else False

{-Questão 9-}
subsequencia (x:xs) (y:ys)
	|(length(x:xs)) > (length(y:ys)) = False
	|take (length(x:xs)) (y:ys) == (x:xs) && (length(x:xs)) <= (length(y:ys)) = True
	|otherwise = subsequencia (x:xs) (ys)

{-Questão 10-}
{-a-}
metade (x:xs) = take (div (length (x:xs)) 2) (x:xs) : [drop (div (length (x:xs)) 2) (x:xs)]

{-b-}
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = (x:xs) ++ (y:ys)
--merge (x:xs) (y:ys) = x : y : merge xs ys

{-swap n m (x:xs)
	| n == x = m : (swap n m xs)
	| m == x = n : (swap n m xs)
	| otherwise = x : (swap n m xs)-}
swap a b = map (\x -> if x == a then b else if x == b then a else x)

{-b-}
minDois (x:xs) (y:ys) = if minimum (x:xs) <= minimum (y:ys) then minimum (x:xs) else minimum (y:ys)
{-Questão 11-}

perfeitos p = [x | x<-[1..p], perfeito x == True]

divisores n = [x | x <-[1..n], n `mod` x  == 0 && n /= x]

perfeito p = if sum (divisores p) == p then True else False

 {--}
menorElem (x:xs) = [minimum (x:xs)] ++ (x:xs)

ordLista (x:xs) = [x] ++ (x:xs)

{-
remover x [] = []
remover e (x:xs) = if
permuta xs = [y:ys | y<-xs, ys<-permuta (remove y xs)]
-}
permutations [] = [[]] 
permutations xs = [y:ys | y <- xs, ys <- permutations (remover y xs) ]
