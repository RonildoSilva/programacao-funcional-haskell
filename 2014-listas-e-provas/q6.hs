----------------------- QUESTAO 06 -----------------------

data Expr = Number Integer
	|Neg Expr
	|Plus Expr Expr
	|Minus Expr Expr
	|Times Expr Expr
	|Div Expr Expr
	|Mod Expr Expr
--a)
n1 = (Number 3)
n2 = (Number 5)

--eval1::Expr->Maybe Integer
--eval1 (Plus (Number a) (Number b)) = Just(a + b)

neg a = Just(a*(-1))

plus a b = Just(a + b)
minus a b = Just(a - b)
times a b = Just(a * b)

divi a b
	|b == 0 = Nothing
	|otherwise =  Just(a / b)

modulo a b
	|b == 0 = Nothing
	|otherwise =  Just(mod a b)