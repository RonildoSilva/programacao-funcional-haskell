module Main where

pegaChar::IO()

pegaChar = do x<-getChar()
	putChar x
	if x=='\n' then return () else main
