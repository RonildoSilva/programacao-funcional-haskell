inserir :: Ord a => a -> [a] -> [a]
inserir x [] =[x]
inserir x (y:ys)
	| x<= y = x:y:ys
	| otherwise = y:inserir x ys
