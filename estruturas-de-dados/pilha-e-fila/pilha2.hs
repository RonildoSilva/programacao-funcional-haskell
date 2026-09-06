import Pilha


size p | isEmpty p = 0
			 | otherwise = 1 + size(pop p)

make [] = empty
make xs = push (make (init xs)) (last xs)
make' xs = foldl push empty xs
