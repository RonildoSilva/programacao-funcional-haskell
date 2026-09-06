-- UNIVERSIDADE FEDERAL DO CEARÁ
-- TRABALHO LINGUAGENS DE PROGRAMAÇÂO
-- RONILDO OLIVEIRA DA SILVA #366763
-- PARTE II - PROGRAMAÇÃO FUNCIONAL

-- Funções Auxiliares

-- Converte um tipo INPUT para um tipo LIST
inputToList :: String -> [Integer]
inputToList entrada = read ("" ++ entrada ++ "")


-- 3 Faça um programa que recebe um vetor de números inteiros e imprime somente os pares.
sPares :: [Integer] -> [Integer]
sPares (x:xs) = if x `mod` 2  == 0 then x:sPares (xs) else sPares (xs)
sPares [] = []

somentePares_ :: IO()
somentePares_ = do
	entrada <- getLine
  	print $ sPares (inputToList entrada)

