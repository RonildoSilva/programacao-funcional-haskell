int2list|x<10 = [x]
	|otherwise = (mod x 10):int2list(div x 10)
	|otherwise = reverse (mod x 10):int2list (div x 10)
	|otherwise = int2list (div x 10)++[mod x 10]
