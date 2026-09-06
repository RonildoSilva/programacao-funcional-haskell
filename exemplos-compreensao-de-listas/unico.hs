unico:: a->[a]->Bool
unico _ []= False
unico x (y:ys) = if x == y && | pertence ys then True
		else unico x ys

