maior3 :: Ord a => a-> a-> a-> a
maior3 a b c = if a >= b && a >= c then a 
			else if b >= a && b>=c then b
			else c