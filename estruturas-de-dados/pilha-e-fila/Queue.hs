module Queue(
	enqueue,
	dequeue,
	front,
	isEmpty,
	empty
)where

data Queue a = Q[a]

enqueue (Q xs) x = Q(xs++[x])

dequeue (Q[]) = error"Vazia" 
dequeue (Q(x:xs)) = Q xs

front (Q(x:xs)) = x

isEmpty (Q[]) = True
isEmpty (Q _) = False

empty = Q[]
