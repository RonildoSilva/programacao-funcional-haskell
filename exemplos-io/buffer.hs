module Main where

square x = x * x

main =	do
	x <-getLine
	show $square(x)
