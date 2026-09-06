module PilhaVet(
	PilhaVet,
	push,
	pop,
	top,
	isEmpty,
	empty
) where

data PilhaVet a = Vet[a] | Vazia

push(Vet xs)x = Vet (x:xs)

pop(Vet []) = error "Vazia"
pop (Vet(x:xs)) = Vet xs

top (Vet(x:xs)) = x

isEmpty (Vet[]) = True
isEmpty _ = False

empty = Vazia
