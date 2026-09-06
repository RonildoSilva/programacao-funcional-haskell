-- UNIVERSIDADE FEDERAL DO CEARÁ
-- TRABALHO LINGUAGENS DE PROGRAMAÇÂO
-- RONILDO OLIVEIRA DA SILVA #366763
-- PARTE II - PROGRAMAÇÃO FUNCIONAL

-- Funções Auxiliares

-- Converte um tipo INPUT para um tipo LIST
inputToList :: String -> [Integer]
inputToList entrada = read ("" ++ entrada ++ "")

-- 8 Faça um programa que recebe uma sequência de números inteiros e imprime essa sequência ordenada.
qSort :: Ord a => [a] -> [a]
qSort (pivo:ls) = qSort esq ++ [pivo] ++ qSort dir
	where esq = [ e | e <- ls, e < pivo]
	      dir = [ d | d <- ls, d >= pivo]
qSort [] = []

quickSort_ :: IO ()
quickSort_ = do
	entrada <- getLine
	print $ qSort (inputToList entrada)
