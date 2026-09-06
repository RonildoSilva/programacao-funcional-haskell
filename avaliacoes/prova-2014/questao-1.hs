data Arv a = Vazia | No a (Arv a) (Arv a)

arv1 = No 15 (No 12 arv2 arv3) (No 20 arv4 arv5)
arv2 = No 10 (Vazia) (Vazia)
arv3 = No 13 (Vazia) (Vazia)
arv4 = No 17 (Vazia) (Vazia)
arv5 = No 23 (Vazia) (Vazia)

--a
nivel _ Vazia = []
nivel 0 (No x esq dir) = [x]
nivel n (No x esq dir) = [x| x <- nivel (n-1) esq ] ++ [y| y <- nivel (n-1) dir ]

--b 
inserir x Vazia = No x (Vazia) (Vazia)
inserir x (No y esq dir)
							| x<y = inserir x esq
							| x>y = inserir x dir
							| otherwise = No x Vazia Vazia dir

--c
mais_esq (No x Vazia _) = x
mais_esq (No _ esq _) = mais_esq esq

--d
listar Vazia = []
listar (No x esq dir) = listar dir ++ [x] ++ listar esq






--6
type Rel = [(Int, Int)]

--a
simetrica [] = True
simetrica[(z,w)] = if z==w then True else False
simetrica ((x,y):xs) = and[ z	| z <- [(elem (y,x) xs)] 
								
