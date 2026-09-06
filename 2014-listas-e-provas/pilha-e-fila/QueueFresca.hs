module QueueFresca(
	enfileira,
	desenfileira,
	front,
	isEmpty,
	empty
)where

data QueueFresca a = Q([a],[a])

enfileira (Q (xs,ys)) x = (Q (xs,(x:ys)))

desenfileira (Q ((x:xs),ys)) = normaliza(Q(xs,ys))

front (Q([],[]))= error"Vazia"
front (Q(x:xs,ys)) = x

isEmpty (Q ([],[])) = True
isEmpty (Q (_ , _ )) = False

empty = (Q([],[]))

normaliza (Q([],ys)) = (Q((reverse ys) , []))

