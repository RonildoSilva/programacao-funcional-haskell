count::(a->Bool)->[a]->Int
count f [] = 0
count f (x:xs)
	|f x = 1 + count f xs
	|otherwise = count f xs

