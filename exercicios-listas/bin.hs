bin::Int->[Int]
--bin a = if mod a 2 == 1 then [1] else bin (div a 2) ++ [0]--
bin a	| (a == 0) = [0]
	| (a == 1) = [1]
	| (mod a 2 == 1) = bin (div a 2) ++ [1]
	| otherwise = bin (div a 2) ++ [0]
