-- UNIVERSIDADE FEDERAL DO CEARÁ
-- TRABALHO LINGUAGENS DE PROGRAMAÇÂO
-- RONILDO OLIVEIRA DA SILVA #366763
-- PARTE II - PROGRAMAÇÃO FUNCIONAL

-- Funções Auxiliares

-- Converte um tipo INPUT para um tipo LIST
inputToList :: String -> [Integer]
inputToList entrada = read ("" ++ entrada ++ "")


-- 2 Faça um programa que recebe um vetor de números inteiros e imprime o produto dos números.
pLista :: [Integer] -> Integer
pLista (x:xs) = x * pLista(xs)
pLista [] = 1

produtoLista_ :: IO()
produtoLista_ = do
	entrada <- getLine
  	print $ pLista (inputToList entrada)

