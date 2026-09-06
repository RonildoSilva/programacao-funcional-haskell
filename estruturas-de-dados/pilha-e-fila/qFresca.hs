import QueueFresca

size q | isEmpty q = 0
			 | otherwise = 1 + size(desenfileira q)
