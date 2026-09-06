----------------------- QUESTAO 02 -----------------------
--1.5
--a)
operador [] n = Nothing
operador xs n
	| n < 0 = Nothing
	| n >= 0 && n <= length xs = Just (xs !! n)
	| otherwise = Nothing

--b)
--getElts::[Int]->[a]->Maybe[a]
--getElts (a:as) (b:bs) = map (operador (b:bs) a) (as) ++ getElts as bs


----------------------- QUESTAO 03 -----------------------

lookup'::Eq a => a -> [(a,b)]->Maybe b
lookup' k ((x,v):assocs)
	| k == x = Just v
	|otherwise = lookup' k assocs
lookup' k  [] = Nothing

telefones :: [(String,String)]
telefones = [("Bob", "01788_665242"),
			 ("Fred", "01688_665242"),
			 ("Alice", "01888_665242")]

emails :: [(String,String)]
emails = [("Bob", "bob@ufc"),
		  ("Fred", "fred@ufc")]

getPhoneEmail name = 
	case lookup' name telefones of
		Nothing -> Nothing
		Just phone -> case lookup' name emails of
			Nothing -> Nothing
			Just email -> Just (phone,email)

--2.0
getPhoneEmailOk name = do{
	a  <- lookup' name telefones;
	b  <- lookup' name emails;
	return(a,b);
}

----------------------- QUESTAO 04 -----------------------
--1.5
--a)
printTimes 0 c = []
printTimes n c = c:[]++printTimes (n-1) c

rld::[(Int, Char)]->String
rld [] = []
rld (x:xs) = printTimes (fst x) (snd x) ++ rld xs

--1.5
--b)
{-

length (rld ps) = sum (map fst ps)

C.B
length (rld []) = sum (map fst [])
length [] = sum (map fst [])
0 = sum (map [])
0 = sum ([])
0 = 0

H.I

length (rld [p]) = sum (map fst [p])
length (rld [(a,b)]) = sum (map fst [(a,b)])
length ([a,...,a]b) = sum (map fst [(a)]b)
a*b = sum ([a,...,a]b)
a*b = sum a*b

*([a,...,a]b) = "a" concatenado b vezes

P.I
length (rld (p:ps)) = sum (map fst (p:ps))
length (rld (p)++rld (ps)) = sum (map fst (p) ++ map fst(ps))
Se length (rld (p)) = sum (map fst (p)) Verdadeiro, entao

length (rld (ps)) = sum (map fst(ps))

-}

----------------------- QUESTAO 05 -----------------------
--2.0
--a)

forM f [] = return ();
forM f (x:xs) = do{
	f x;
	forM f (xs);
}

----------------------- QUESTAO 06 -----------------------







