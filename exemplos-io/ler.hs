module Main where

soma 0 s = putStrLn (show s)

soma n s = 	do
			x<-getLine
			soma (n-1) (s+read x)

main = do
	x<-getLine
	soma (read x) 0