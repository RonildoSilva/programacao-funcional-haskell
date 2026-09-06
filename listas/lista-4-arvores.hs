data Arv = Vazia | No Int Arv Arv deriving (Show)


arv1 = Vazia
arv2 = No 2 Vazia Vazia
arv3 = No 2 (No 3 Vazia Vazia) (No 4 Vazia Vazia)

reflect Vazia = Vazia
reflect (No x esq dir) = No x (reflect dir) (reflect esq)

--inserir :: Ord a => a -> Arv -> Arv
inserir x Vazia = No x Vazia Vazia
inserir x (No y esq dir)
	|x < y = No y (inserir x esq) dir
	|x > y = No y esq (inserir x dir)
	|otherwise = No y esq dir

mais_esq :: Arv->Int
mais_esq (No x Vazia _) = x
mais_esq (No x esq dir) = mais_esq esq
mais_esq Vazia = error "Ervaro Vazia"

remover x Vazia = Vazia
remover x (No y Vazia dir)
	|x == y = dir
	|x < y = (No y Vazia dir)
	|otherwise = (No y (Vazia) (remover x dir))

data Prop = Const Bool
	|Var Char
	|Neg Prop
	|Conj Prop Prop
	|Disj Prop Prop
	|Impl Prop Prop


instance Show Prop where 
	show p = showProp p

prop1 = (Neg (Var 'a'))
prop2 = (Disj (prop1) (Neg prop1))

dual::Prop->Prop
dual (Const True) = (Const False)
dual (Const False) = (Const True)
dual (Var a) = (Var a)
dual (Neg p) = (Neg (dual p))
dual (Conj p1 p2) = Disj (dual p1) (dual p2)
dual (Disj p1 p2) = Conj (dual p1) (dual p2)
dual (Impl p1 p2) = dual (Disj (Neg p1) (p2))

variaveis::Prop->[Char]
variaveis (Const x) = []
variaveis (Var x) = [x]
variaveis (Neg p) = variaveis p

showProp::Prop->String
showProp(Const True) = "T"
showProp(Const False) = "F"
showProp(Var a) = a:[]
showProp(Neg p) = "¬" ++ showProp p
showProp(Disj p1 p2) = showProp p1 ++ " v " ++ showProp p2
showProp(Conj p1 p2) = showProp p1 ++ " ^ " ++ showProp p2
showProp(Impl p1 p2) = showProp p1 ++ " -> " ++ showProp p2


