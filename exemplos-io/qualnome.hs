module Main where

saudacao :: IO()

saudacao = do
	putStr "Qual nome?"
	nome <-getLine
	putStr ("OI" ++ nome ++ "!\n")

main = do
	saudacao	