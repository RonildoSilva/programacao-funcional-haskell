--
partes [] = [[]]
partes (x:xs) = partes xs ++ map(x:) (partes xs)
--
{-7-}
data Mobile = Pendente Int | Barra Mobile Mobile

--peso::Mobile->Int
peso (Pendente p) = p
peso (Barra m1 m2) = peso m1 + peso m2

--balanceado::Mobile->Bool
balanceado (Pendente p) = True
balanceado (Barra m1 m2) = if peso m1 == peso m2 then True else False

{-8-}
--splits::[a]->[([a],[a])]
lista = [4,5,1,2]

permuta ([a],[b]) = [([a],[b])]++[([b],[a])]


--splits [] = [([],[])]
--splits (x:xs) = [((head (partes (x:xs))),(last (partes (x:xs))))] ++ splits xs

deleteAll xs ys = [y | y<-ys, notElem y xs]


splits [] = [([],[])]
splits xs = [(x, deleteAll x xs) | x<-partes xs]

eqsplits xs = [(x,y) | (x,y)<-splits xs,sum x == sum y]

--makeBMobile::[Int]->Maybe Mobile
--[([3],[7])]++[([2],[3])]
