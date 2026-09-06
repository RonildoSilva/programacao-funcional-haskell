lenght::[a]->Int
lenght [] = 0
lenght xs = 1+lenght init xs
