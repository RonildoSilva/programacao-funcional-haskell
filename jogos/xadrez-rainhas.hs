import Data.List

type Pos = (Int,Int)

ataca::Pos->Pos->Bool
ataca (x,y) (z,w) |x==z = True
	|y==w = True
	|x-y == z-w = True
	|x+y == z+w = True

posOk::Pos->[Pos]->Bool
posOk x [] = True
posOk x (y:ys) = not (ataca x y) && posOk x ys

safe::[Pos]->Bool
safe [] = True
safe (x:xs) = posOk x xs && safe xs

rainha n = [zip xs [1..n] | xs <-permutations[1..n], safe (zip xs [1..n])]

intercala x [] = [[x]]
intercala x (y:ys) = (x:y:ys) : [y:z|z<-intercala x ys]

permutacao [] = [[]]
permutacao (x:xs) = [ y | z<-permutacao xs, y<-intercala x z]
--permutacao (x:xs) = [intercala x z | z<-permutacao xs, y<-intercala x z]

data Op = Sum|Sub|Mul|Div

ops = [Sum,Sub,Mul,Div]

valida::Op->Int->Int->Bool
valida Div a b | b /= 0 = True
valida _ _ _ = True

aplica::Op->Int->Int->Int
aplica Sum x y = x+y
aplica Sub x y = x-y
aplica Mul x y = x*y
aplica Div x y = div x y

data Expr = Num Int | Apl Op Expr Expr

--deriving (Show)
--instance Show Expr where
--	show = showExpr

valor::Expr->[Int]
valor (Num n)=[n]
valor (Apl op e1 e2)=[aplica op v1 v2 | v1<- valor e1, v2<- valor e2, valida op v1 v2]

splits::[a]->[([a],[a])]
splits [] = []
splits [_] = []
splits (x:xs) = ([x],xs):[(x:is,ds) | (is, ds)<-splits xs]

combina::Expr->Expr->[Expr]
combina e1 e2 = [Apl op e1 e2 | op<-ops]

expressoes [] = []
expressoes [n]=[Num n]
expressoes xs = [e|(is,ds)<-splits xs, e1<-expressoes is,e2<-expressoes ds,e<-combina e1 e2]