maior3 a b c = if a >= b && a >= c then a 
	       else if b >= a && b>=c then b
	       else c
triangulo a b c 
	|maior3 a b c <= a+b+c-maior3 a b c = True
	|otherwise = False
