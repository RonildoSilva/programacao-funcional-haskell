pertence :: a->[a]->Bool
pertence _ [] false
pertence x (y:ys) = if x==y then True
		else pertence x ys
