{-quad-}
quad = do
	x <- getLine
	putStrLn (show (read x * read x))

{-1-}
palindromoIO::IO()
palindromoIO = do
	putStrLn "Informe uma frase"
	frase <- getLine
	if ((reverse (frase)) == frase)then putStrLn "Palindromo"
	else putStrLn "Não Palindromo"	

{-2-}
fat :: Int->Int
fat 0 = 1
fat 1 = 1
fat a = a * fat (a-1)

fatIO::IO()
fatIO = do
	putStrLn "Entre com um numero"
	num <- getLine
	print (fat (read num))

{-3-}
primos :: Int -> Bool
primos a = if length [ x | x <- [1..a], mod a x == 0] == 2 
	then True else False

primoIO::IO()
primoIO = do
	putStrLn "Entre com um inteiro"
	num <- getLine
	print (primos (read num))

{-4-}
lerNum::IO()
lerNum = do
	putStrLn "Entre com 1o numero"
	um <- getLine
	putStrLn "Entre com 2o numero"
	dois <- getLine
	putStrLn "Entre com 3o numero"
	tres <- getLine
	print ((read um :: Float)*(read dois :: Float)*(read tres :: Float))

{-5-}
convFtoC::Double->Double
convFtoC f = (f-32)*(5/9)

convertTemp::IO()
convertTemp = do
	putStrLn "Enre com a temperatura"
	f <- getLine
	print (convFtoC (read f :: Double))

{-6-}
credito::IO()
credito = do
	putStrLn "Salario: "
	sal <- getLine
	putStrLn "Prestação: "
	prest <- getLine
	if (read prest :: Float) < (read sal :: Float) * 0.3 then
		print "Empréstimo Concedido"
	else
		print "Empréstimo Não Concedido"

{-7-}
setima = do{
	test <-getLine;
	contents<-getContents;
	putStrLn $ show $take (read test) (lines contents)
}

--sum(map (read) (take(read test)))

{-8-}
{-
oitava = do{
	contents<-getContents;
	putStrLn "Linhas "++ show(length(lines contents))
	putStrLn "Palavras " ++ show(sum(map length(map words (lines contents))))
}
-}


{-12-}
data Prop = Const Bool
	|Var Char
	|Neg Prop
	|Conj Prop Prop
	|Disj Prop Prop
	|Impl Prop Prop

instance Show Prop where show p = showProp p

dual::Prop->Prop
dual (Const True) = (Const False)
dual (Const False) = (Const True)
dual (Var a) = (Var a)
dual (Neg p) = (Neg (dual p))
dual (Conj p1 p2) = Disj (dual p1) (dual p2)
dual (Disj p1 p2) = Conj (dual p1) (dual p2)
dual (Impl p1 p2) = dual (Disj (Neg p1) (p2))

-- Exemplo
p1 = (Neg (Var 'p'))
p2 = (Var 'q')
p3 = (Neg (Var 'q'))

p4 = (Disj (p2) (Neg p3))
p5 = (Conj (p3) (p4))
p6 = (Disj (p4) (Neg p5))
p7 = (Disj (p1) (Neg p6))
p8 = (Impl (Conj (Neg p3) (Neg p4)) (Disj (p1) (Neg p6)))
{-13-}

--contar::Prop->[(Char,Int)]

contar (Var p) = [('p',1)]
contar (Neg p) = [('p',2)]
contar (Disj p q) = contar p ++ contar q
contar (Conj p q) = contar p ++ contar q
contar (Impl p q) = contar p ++ contar q

{-14-}
showProp::Prop->String
showProp(Const True) = "T"
showProp(Const False) = "F"
showProp(Var a) = a:[]
showProp(Neg p) = "~" ++ showProp p
showProp(Disj p1 p2) = showProp p1 ++ " v " ++ showProp p2
showProp(Conj p1 p2) = showProp p1 ++ " ^ " ++ showProp p2
showProp(Impl p1 p2) = showProp p1 ++ " -> " ++ showProp p2

{-15-}
satifaz::Prop->Bool
satifaz (Const True) = True
satifaz (Const False) = False
satifaz (Var a) = True
satifaz (Neg a) = False
satifaz (Disj p1 p2) = satifaz p1 || satifaz p2
satifaz (Conj p1 p2) = satifaz p1 && satifaz p2
satifaz (Impl p1 p2) = satifaz (Neg p1) || satifaz p2

{-17-}
data Arv = Vazia | No Int Arv Arv deriving (Show)

arv1 = Vazia
arv2 = No 2 Vazia Vazia
arv3 = No 2 (No 3 Vazia Vazia) (No 4 Vazia Vazia)
arv4 = No 5 arv3 arv2

{-a-}
tamanho :: Arv -> Int
tamanho Vazia = 0
tamanho (No x esq dir) = tamanho (esq) + tamanho(dir) + 1

{-b-}
altura :: Arv -> Int
altura Vazia = 0
altura (No x esq dir) = (max (altura  esq)  (altura dir)) + 1

{-c-}
sumArv :: Arv -> Int
sumArv Vazia = 0
sumArv (No x esq dir) = sumArv esq + sumArv dir + x

{-d-}
nivel::Int->Arv->[Int]
nivel _ Vazia = []
nivel 0 (No x esq dir) = [x]
nivel n (No x esq dir) = nivel (n-1) (esq) ++ nivel (n-1) (dir)

{-e-}
mapArv :: (Int->Int) -> Arv -> Arv
mapArv f Vazia = Vazia
mapArv f (No x esq dir) = No (f x) (mapArv f esq) (mapArv f dir)

{-f-}
reflect::Arv->Arv
reflect Vazia = Vazia
reflect (No x esq dir) = No x (reflect dir) (reflect esq)

{-g-}
inserir::Int->Arv->Arv
inserir x Vazia = No x Vazia Vazia
inserir x (No y esq dir)
	|x < y = No y (inserir x esq) dir
	|x > y = No y esq (inserir x dir)
	|otherwise = No y esq dir

{-h-}
mais_esq :: Arv->Int
mais_esq (No x Vazia _) = x
mais_esq (No x esq dir) = mais_esq esq
mais_esq Vazia = error "Ervaro Vazia"

{-i-}
listar::Arv->[Int]
listar Vazia = []
listar (No x esq dir) = [x] ++ listar esq ++ listar dir

busca::Int->Arv->Bool
busca n Vazia = False
busca n (No x esq dir) = True

{-19-}
type Rel = [(Int, Int)]

tupla = [(1,1),(2,2),(1,2),(3,3)]

fs::([Integer],[Integer])->[Integer]
fs ([],_) = []
fs ((a:as),_) = (a:as)

sd::([Integer],[Integer])->[Integer]
sd (_,[]) = []
sd (_,(a:as)) = (a:as)

--comp::[Integer]->[Integer]->[Integer]

compara = [ (b) | a <- (fs $unzip tupla), b <- (sd $unzip tupla), a == b] 

ehReflexivo xs ys = and [elem x ys | x<-xs]
--reflexiva::Rel->Bool
--reflexiva 


