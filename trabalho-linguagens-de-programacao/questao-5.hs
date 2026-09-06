-- UNIVERSIDADE FEDERAL DO CEARÁ
-- TRABALHO LINGUAGENS DE PROGRAMAÇÂO
-- RONILDO OLIVEIRA DA SILVA #366763
-- PARTE II - PROGRAMAÇÃO FUNCIONAL

-- Funções Auxiliares

-- Converte um tipo INPUT para um tipo LIST
inputToList :: String -> [Integer]
inputToList entrada = read ("" ++ entrada ++ "")


-- 5 Faça um programa que recebe duas listas A e B e imprime a intersecção de A e B.
inter [] a = []
inter a [] = []
inter (a:as) (b:bs)
	| a < b = inter as (b:bs)
	| a == b = a : inter as bs
	| otherwise = inter (a:as) bs

inter_ ::IO()
inter_ = do
	lista1 <- getLine
	lista2 <- getLine
  	print $ (inter (inputToList lista1) (inputToList lista2))


