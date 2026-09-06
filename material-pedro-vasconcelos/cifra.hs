{- 
   A cifra de César em Haskell
   Pedro Vasconcelos, 2010
   baseado na solução em  "Programming in Haskell",  Graham Hutton, 2007
-}
import Data.Char (chr, ord)

-- Converte letras em inteiros 0..25 e vice-versa
let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char 
int2let n = chr (n+ord 'a')

minuscula :: Char -> Bool
minuscula x = x>='a' && x<='z'

-- Efectuar um deslocamento de k posições
desloca :: Int -> Char -> Char
desloca k x | minuscula x = int2let ((let2int x+k)`mod`26)
            | otherwise   = x

-- Repetir o deslocamento para toda a cadeia de caracteres.
cifrar :: Int -> String -> String
cifrar k xs = [desloca k x | x<-xs]



{-
---------------------------------------------------------
 Quebrar a cifra de César usando contagem de frequências
---------------------------------------------------------
-}

-- Frequências das letras em português
tabela :: [Float]
tabela = [13.9, 1.0, 4.4, 5.4, 12.2, 1.0,
          1.2, 0.8, 6.9, 0.4, 0.1, 2.8, 4.2,
          5.3, 10.8, 2.9, 0.9, 6.9, 7.9, 4.9,
          4.0, 1.3, 0.01, 0.3, 0.01, 0.4]


-- Contar ocorrências numa lista
contar :: Eq a => a -> [a] -> Int
contar x xs = length [x' | x'<-xs, x'==x]

-- Contar as minúsculas numa cadeia
minusculas :: String -> Int
minusculas xs = length [x | x<-xs, minuscula x]

-- Converter um rácio numa percentagem [0,100]
porcento :: Int -> Int -> Float
porcento n m = (fromIntegral n / fromIntegral m) * 100

-- Contar frequencias de um texto
freqs :: String -> [Float]
freqs xs =  [porcento (contar x xs) n | x<-['a'..'z']]
    where n = minusculas xs

-- Calcular a medida estatística chi-quadrado
chiquad :: [Float] -> [Float] -> Float
chiquad os es = sum [((o-e)^2)/e | (o,e)<-zip os es]

-- Rotação circular duma lista
rodar :: Int -> [a] -> [a]
rodar n xs = drop n xs ++ take n xs

-- Indices de ocorrências de um valor numa lista
indices :: Eq a => a -> [a] -> [Int]
indices x ys = [i | (i,y)<-zip [0..n] ys, x==y]
    where n = length ys - 1

-- Quebrar a cifra de César
quebrar :: String -> String
quebrar xs = cifrar (-k) xs
    where 
      obs = freqs xs
      chitab = [chiquad (rodar k obs) tabela | k<-[0..25]]
      k = head (indices (minimum chitab) chitab)

{- Alguns desafios
   autoria: Rogério Reis <rvr@dcc.fc.up.pt>
-}
-- espaços e pontuação preservados do texto original
desafio1 = "jcnxyjr xjruwj ywjx qfitx szrf inxhzxxft: t rjz qfit, t yjz qfit j t qfit htr wfeft!"

-- espaços e pontuação removidos; letras agrupadas em blocos de 5
desafio2 = "qgzma fqzta yqpap myadf qeazm acgqd aqefm dbdqe qzfqz qeeqy ayqzf a"
