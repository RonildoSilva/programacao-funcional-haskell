{-1-}
--Maybe a = Nothing | Just a

safeLog x 
	|x > 0 = Just (log x)
	|otherwise = Nothing

safeSqrt x
	|x > 0 = Just  (sqrt x)
	|otherwise = Nothing

--log o sqrt



--safeLogSqrt x = safeLog(safeSqrt x)
--Maybe Flooting

safeLogSqrt x = safeSqrt x >>= safeLog


--Just x >>= f = f x
--Nothing >>= f = Nothing
safeLogSqrt' x = do{
	y<-safeSqrt x;
	z<-safeLog y;
	return z;
}
{-5-}

type Person = String
type Family = [(Person, Person, Person)]

p1 = "Bart_Simpson"
p2 = "Lisa_Simpson"
p3 = "Marge_Simpson"
p4 = "Homer_Simpson"
p5 = "Maggie_Simpson"
p6 = "Abraham_Simpson"
p7 = "Mona_Simpson"
p8 = "Ned_Flanders"
p9 = "Maude_Flanders"
p10 = "Rod_Flanders"
p11 = "Todd_Flanders"

f = [(p4,p3,p1),
	 (p4,p3,p2),
	 (p4,p3,p5),
	 (p6,p7,p4),
	 (p8,p9,p10),	
	 (p8,p9,p11)]

{-a-}
father::Family->Person->Maybe Person
father ((p,m,f):fs) x
	|x == f = Just p
	|otherwise = father fs x
father [] x = Nothing	

{-b-}
mother::Family->Person->Maybe Person
mother ((p,m,f):fs) x
	|x == f = Just m
	|otherwise = mother fs x
mother [] x = Nothing	

{-c-}
paternalGrandFather::Family->Person->Maybe Person
paternalGrandFather f p = father f p >>= father f

{-d-}
paternalGrandMother::Family->Person->Maybe Person
paternalGrandMother f p = father f p >>= mother f

{-e-}
bothGrandFathers::Family->Person->Maybe (Person,Person)
bothGrandFathers f p = do
	m <- paternalGrandMother f p
	p <- paternalGrandFather f p
	return (m,p)

data Ponto = Pt Float Float
type Regiao = Ponto -> Bool

data Grafo = G[Vert]
caminho _ [] = True
caminho (G((x:xs),_)) [a] = True
caminho g@(G(v,cs)) (w1:w2:ws) = if elem (w1,w2) cs then caminho g (w2:ws) else False

