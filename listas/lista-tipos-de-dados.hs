-- Tipos de Dados

-- Q1
type Ponto' = (Float, Float)

--a
dist :: Ponto' -> Ponto' -> Float
dist (x1,y1) (x2,y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

--b
comprimento :: [Ponto'] -> Float
comprimento [] = 0.0
comprimento [(x,y)] = 0.0
comprimento (p1:p2:ps) = (dist p1 p2) + comprimento ps

-- Q2
data Prop = Var Char 
		| Neg Prop
		| Conj Prop Prop
		| Disj Prop Prop

p1 = (Conj (Var 'a') (Disj (Var 'a') (Var 'b')))

contar :: Prop -> [(Char, Int)]

contar p = contar'(contar'' p)

contar'' (Var a) = [(a, 1)]
contar'' (Neg p) = contar'' p   
contar'' (Conj p1 p2) = contar'' p1 ++ contar'' p2 
contar'' (Disj p1 p2) = contar'' p1 ++ contar'' p2

contar' [] = []
contar' [(a,n)] = [(a,n)]
contar' ((a1,n1):(a2,n2):xs) = if a1 == a2 then (a1, n1+n2):contar' xs 
								else (a1,n1):(a2,n2):contar' xs

-- Q3
data Mobile = Haste Mobile Int Mobile Int | Objeto Int

m1 = Haste (Objeto 1) 6 (Objeto 3) 2
m2 = Haste (Objeto 2) 4 (Objeto 4) 2
m3 = Haste (Objeto 1) 1 (Objeto 1) 1
m4 = Haste (m3) 3 (m2) 1
m5 = Haste (m4) 2 (m1) 4

peso :: Mobile -> Int
peso (Objeto x) = x
peso (Haste m1 d1 m2 d2) = peso m1 + peso m2

equilibrio :: Mobile -> Bool
equilibrio (Objeto x) = True
equilibrio (Haste m1 d1 m2 d2) = if (peso m1) * d1 ==  (peso m2) * d2 then and [equilibrio m1, equilibrio m2]
								else False

-- Q4

data MConj a = Vazio | No a Int (MConj a) (MConj a) deriving Show

-- a
ocorre :: Ord a => a -> MConj a -> Int
ocorre x Vazio = 0
ocorre x (No y z esq dir) = if x==y then z
							else if x < y then ocorre x esq
							else ocorre x dir

--b
inserir :: Ord a => a -> MConj a -> MConj a
inserir a Vazio = No a 1 Vazio Vazio
inserir a (No y z esq dir) = if a==y then (No y (z+1) esq dir)
							else if a < y then inserir a esq
							else inserir a dir
 
-- Q5

data Ponto = Pt Float Float
type Regiao = Ponto -> Bool

--a 
--retang::Ponto->Ponto->Regiao
retang (Pt x1 y1) ( Pt x2 y2) = (\(Pt x y)->(x>=x1 && x<=x2 && y>=y1 && y<=y2))

distancia (Pt x1 y1) (Pt x2 y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)
--circ::Ponto->Raio->Regiao
circ (Pt x1 y1) r = (\(Pt x y) -> (distancia (Pt x1 y1) (Pt x y)) <= r)

--b


-- Q6
type Vert = Int
type Grafo = ( [Vert], [(Vert, Vert)])

-- caminho -> Grafo -> [Vert] -> Bool
caminho _ [] = True
caminho _ [x] = True
caminho g@(v,e)(x:y:xs) = elem (x,y) e && caminho g xs

