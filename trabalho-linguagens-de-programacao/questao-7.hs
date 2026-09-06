-- UNIVERSIDADE FEDERAL DO CEARÁ
-- TRABALHO LINGUAGENS DE PROGRAMAÇÂO
-- RONILDO OLIVEIRA DA SILVA #366763
-- PARTE II - PROGRAMAÇÃO FUNCIONAL

-- Funções Auxiliares

-- Converte um tipo INPUT para um tipo LIST
inputToList :: String -> [Integer]
inputToList entrada = read ("" ++ entrada ++ "")


-- 7 Faça um programa que determina se um dado número inteiro é primo. A saída deve ser apenas as palavras sim ou nao.
tamList :: Num a => [t] -> a
tamList (x:xs) = 1 + tamList(xs)
tamList [] = 0

nDiv :: Integral t => t -> [t]
nDiv a = [i | i<-[1..a], a `mod` i == 0]

nPrim :: Integral t => t -> [Char]
nPrim a = if tamList (nDiv a) == 2 then "sim" else "nao"

numeroPrimo_ :: IO ()
numeroPrimo_ = do
	entrada <- getLine
	print $ nPrim (read entrada)

