type String = [Char]

Class Num
(+), (-), negate, abs, signum, fromInteger

data Z = Zero | Succ z | Pred Z

toInt::Z->Interger
toInt(Zero) = 0
toInt(Succ x) = (toInt x) + 1
toInt(Pred x) = (toInt x) - 1

z1 = Zero
z2 = Succ z1
z3 = Succ z2

>z3
Succ (Succ Zero)

instance Show Z where
	Show x = show(toInt z)

show::Z->String

>z3
2

fromInt::Int->Z
fromInt 0 = Zero
fromInt x 	| x > 0 = (Succ fromInt(x-1))
			| x < 0 = (Succ fromInt(x+1))


instance Num Z where
	z1+z2 = fromInt((toInt z1)+(toInt z2))
	z1*z2 = fromInt((toInt z1)*(toInt z2))

	negate z = fromInt(negate(toInt z2))
	abs z = fromInt(abs (toInt z))
	signum z = fromInt(signum (toInt z))
	fromIntegerz = fromInt

---------------------------------------------

data Fraction = Q Interger Interger
	Show(Q 2 3) = "2/3"

instance Show Fraction where
	Show (Q xy) = Show x ++ "/" Show y

instance Num Fraction where
	(Q xy) + (Q x' y') = Q (y' ++ x' y') (y y')

Polinomio = PolZero | Coef Float Int Polinomio

Coef 2 2 (Coef 4 1 (Coef 5 0) PolZero)
--2x² + 4x + 5
--2x^2 + 4x^1 + 5x^0

-------------------

instance Show Polinomio where
	Show (Coef a n p)=
		Show a ++ "x^" ++ Show ++ "+" ++ Show p
		Show PolZero = "0"

-------------------

grauPolinomio::Polinomio->Int
grauPolinomio PolZero = 0
grauPolinomio (Coef a n p) = n

-------------------

somaTermo b n PolZero = Coef b n  PolZero
somaTermo b n (Coef a x p) 
	|n==x = Coef(a+b) (n) p
	|otherwise = Coef ax+somaTermo b n p

-------------------

somaTermo b n (PolZero)	= (Coef b n PolZero)
somaTermo b n (Coef x y p) = if n== y then (Coef (b+x) y p)
	else if n > y then
		(Coef b n (Coef x y p))
	else
		(Coef x y (somaTermo b n p))

-------------------

multTerm b n PolZero = PolZero
multTerm b n (Coef a x p) = Coef b*a n+x (multTerm b n p)

somaPoly (Coef a n p) q = somaTermo a n (somaPoly p q)
somaPoly PolZero q = q
somaPoly (Coef a n p) q = somaPoly p (somaTermo a n q)
somaPoly p q = p + q


multTerm b n p = bx^n * p
multPoly p q = p + q
avaliarPoly p a = p(a)
