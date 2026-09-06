--liftM::Monad m
liftM f m1 = do {
	x1 <- m1;
	return (f x1);
}

--a)
mapM' f [] = return []
mapM' f (x:xs) = do {
	y <- f x;
	ys <- mapM' f xs;
	return (y:ys);
}


--b)
mapM_' f [] = return ();
mapM_' f (x:xs) =  do {
	f x;
	mapM_' f xs;
}

--7
replace i x [] = []
replace i x (ys) = take i ys ++ [x] ++ drop (i+1) ys

--8
wat [] n = Nothing
wat xs n = if n < length xs then Just (xs !! n)
			else Nothing	

myUnzip [] = ([], []) -- Defaults to a pair of empty lists, not null
myUnzip xs = (map fst xs, map snd xs)


tupla = [(1,2),(5,7),(9,4)]
tipla = [1,5,9]
tapla = [2,7,4]

mona = getLine >>= \x->putStrLn ("Você digitou: " ++ x)

partes [] = [[]]
partes (x:xs) = [x : y | y <- partes xs ] ++ partes xs

--9
soma [] = 0
soma(x:xs) = x + soma xs
--a
somaAcc xs z = soma xs + z
--xs = []

natural 0 = Nothing
natural a
	|a < 0 = Nothing
	|otherwise = Just (a)
