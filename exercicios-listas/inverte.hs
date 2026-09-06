inverte::[a]->[a]
inverte [] = []
inverte xs = last xs: inverte (init xs)
