frequencia :: a-> [a] -> [Int]
frequencia _ [] = 0
frequencia x (y:ys) = if x==y then 1+frequencia x ys
			else frequencia x ys
