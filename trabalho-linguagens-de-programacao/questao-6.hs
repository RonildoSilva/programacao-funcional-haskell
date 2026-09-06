-- UNIVERSIDADE FEDERAL DO CEARÁ
-- TRABALHO LINGUAGENS DE PROGRAMAÇÂO
-- RONILDO OLIVEIRA DA SILVA #366763
-- PARTE II - PROGRAMAÇÃO FUNCIONAL

-- Funções Auxiliares

-- 6 Faça um programa que determina se uma dada string é palíndroma. A saída deve ser apenas as palavqras sim ou nao.
--reverso :: [Integer] -> [Integer]
reverso (x:xs) = reverso (xs) ++ [x]
reverso [] = []

--pDromo :: [Integer] -> [Char]
pDromo (x:xs) = if (x:xs) == reverso (x:xs) then "sim" else "nao"

--palindromo_ :: IO()
palindromo_ = do
	entrada <- getLine
	print $ pDromo (entrada)

