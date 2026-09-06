--1 
--a) Eq t => t-> [t] -> [t]
remove x[] = []
remove x (y:ys) = if x==y then ys else y: (remove x ys)
--b) [t] -> [[t]]
partes [] = [[]]
partes (x:xs) = [x:y | y<- partes xs] ++ partes xs
--c) Int->[a]->[a]
rota n xs = drop n xs ++ take n xs
--d) (t1,t) -> (t,t1)
swap(x,y) = (y,x)
--e) (t->t)->t->t
twice f x = f(f x)
--2 a) Remove um número X dentro da lista
--b) Recebe uma lista e transforma cada elemento da lista em um conjunto das partes da lista
--c) pega o numero de termos passados e coloca no final da lista
--d) troca dois elementos
--e) aplica uma função passada duas vezes
--3 
--a)	Int-> Int-> Int
proMn m n = product [m..n]
--b) Int -> Int
dobro n = 2*n
sumQuad n = sum(map dobro [0..n])
--c) [a] ->[a] 
interior xs = drop 1 (take ((length xs)-1) xs)
--d) Int-> Int -> [a] -> [a]
segmento m n xs = [(!!) xs x | x<-[(m-1)..(n-1)] ]
--4 
--a) Eq a=> [a] -> [a] -> Bool
subconjunto xs ys = and [elem x ys | x<-xs]
--b) Eq a=>a [a] -> [a] -> [a]
union xs ys = xs ++ [x| x<-ys, notElem x xs] 
--c) Eq a => [a] -> [a] -> [a]
diferencia xs ys = [x| x<-xs, notElem x ys]
--5)
aproximaPi n = sqrt(6*sum[1/(x^2)| x<-[1..n]])
--6) [Integer] -> Integer
--maiorSalto xs = max[x-y| ]
--7) 
--a) a -> [a] -> Bool
elem' x xs = foldr(\y ys -> if x == y then True else ys) False xs
--b) Eq a => [a] -> [a]
--remdups xs = foldr
--c) (a -> Bool) -> [a] -> Bool
todos p xs = foldr(\x ys-> if p x && ys then True else False)True xs
--e) (a->Bool) -> [a] -> Bool
algum p xs = foldr(\y ys -> if p y || ys then True else False) False xs
--8)	Eq a => [a] -> [a] -> Bool
prefix xs ys = if xs == (take (length xs) ys) then True else False
-- Função Map com Foldr
map' f xs = foldr(\x ys-> (f x):ys) [] xs
--11) Int -> [Int]
perfeito n = (sum[x| x<-[1..(n-1)], (mod n x) == 0]) == n
perfeitos n = [x| x<-[1..n], perfeito x]
--12) [a] -> [[a]]
permutations [] = [[]]
permutations xs = [y:ys| y<-xs, ys<- permutations(remove y xs)]


