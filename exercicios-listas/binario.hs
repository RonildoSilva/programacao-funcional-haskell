{-
2356
6
2350/10
-}


int2list::Int->[Int]
int2list a = if a < 10 then [a] else [mod a 10] ++ int2list (div a 10)

soma::[Int]->Int
soma [] = 0
soma (x:xs) = x + soma xs
{-soma (x:xs) = x + soma xs-}
