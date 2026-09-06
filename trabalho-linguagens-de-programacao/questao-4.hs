-- UNIVERSIDADE FEDERAL DO CEARÁ
-- TRABALHO LINGUAGENS DE PROGRAMAÇÂO
-- RONILDO OLIVEIRA DA SILVA #366763
-- PARTE II - PROGRAMAÇÃO FUNCIONAL

-- Funções Auxiliares

-- Converte um tipo INPUT para um tipo LIST
inputToList :: String -> [Integer]
inputToList entrada = read ("" ++ entrada ++ "")


-- 4 Faça um programa que recebe duas listas A e B e imprime a união de A e B.
qSort :: Ord a => [a] -> [a]
qSort (pivo:ls) = qSort esq ++ [pivo] ++ qSort dir
	where esq = [ e | e <- ls, e < pivo]
	      dir = [ d | d <- ls, d >= pivo]
qSort [] = []

remove_dups :: (Ord a, Eq a) => [a] -> [a]
remove_dups xs = remove $ qSort xs
  where
    remove []  = []
    remove [x] = [x]
    remove (x1:x2:xs)
      | x1 == x2  = remove (x1:xs)
      | otherwise = x1 : remove (x2:xs)

concatena :: [a] -> [a] -> [a]
concatena (x:xs) (y:ys) = [x] ++ [y] ++ concatena xs (ys)
concatena [] [] = []
concatena [] [x] = [x]
concatena [y] [] = [y]

uniao :: Ord a => [a] -> [a] -> [a]
uniao (x:xs) (y:ys) = remove_dups (concatena (x:xs) (y:ys))

uniao_ ::IO()
uniao_ = do
	lista1 <- getLine
	lista2 <- getLine
  	print $ (uniao (inputToList lista1) (inputToList lista2))

