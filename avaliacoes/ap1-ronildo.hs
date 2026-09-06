{-Questao 1-}
{-a-}
final n xs = drop n xs

{-b-}
rota n xs = drop n xs ++ take n xs

{-Q2-}
{-a-}
inter xs ys = [x | x<-xs, elem x ys ]

{-b-}
diferencia xs ys = [x | x<-xs, notElem x ys ]

{-Q3-}
{-a-}
listaVog = ['a','e','i','o','u','A','E','I','O','U']
dupli a = if elem a listaVog then [a,a] else [a]

duplicar [] = []
duplicar (x:xs) = dupli x ++ duplicar xs

{-b-}
aplica [x] n = [x n]
aplica (x:xs) n = [x n] ++ aplica xs n

{-Q4-}
{-a-}
paridade xs = if length (filter (==True) xs) `mod` 2 == 0 then False else True

--foldr (&&) False [True ,True  ,True ]


{-Q05-}
media [x]  = x
media (x:xs) = x + media(xs)

acumula [x] = 1
acumula (x:xs) = 1 + acumula xs

intercala x [] = []
intercala x (y:ys) = if (length (y:ys) <2) then (y:ys) else y : x : intercala x ys

--intercala x (y:ys) = y : x : intercala x ys


pari [True] = 1
pari [False] = 0
--pari (x:xs) = if pari (x:xs) `mod` 2 == 0 then False else pari [x] + pari xs
pari (x:xs) = pari [x] + pari xs
--if pari (x:xs) `mod` 2 == 0 then False else pari [x] + pari xs