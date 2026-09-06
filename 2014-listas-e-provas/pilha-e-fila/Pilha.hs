--Omitimos a representação dos Dados

module Pilha(
	Pilha,
	push,
	pop,
	top,
	isEmpty,
	empty
) where

data Pilha a = Vazia | Topo a (Pilha a)
											deriving(Show)

push p x = Topo x p

pop Vazia = error "Pilha Vazia"
pop (Topo x p) = p

top Vazia = error "Pilha Vazia"
top (Topo x p) = x

isEmpty Vazia = True
isEmpty _ = False

empty = Vazia
