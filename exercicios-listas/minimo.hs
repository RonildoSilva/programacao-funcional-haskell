minimo::[Int]-> Int
minimo [x] = x
minimo (x:xs) = if x < minimo xs then x
		else minimo xs
