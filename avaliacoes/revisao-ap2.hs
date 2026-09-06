{-IO-}

{-3-}
--                  Distancia  Peso   
data Mobile = Haste Mobile Int Mobile Int | Objeto Int

--m1 = Haste (Objeto 8) 6 (Objeto 3) 2
m1 = Haste (Objeto 1) 6 (Objeto 3) 2 --peso 4 
m2 = Haste (Objeto 2) 4 (Objeto 4) 2
m3 = Haste (Objeto 1) 1 (Objeto 1) 1
m4 = Haste (m3) 3 (m2) 1 -- peso 8
m5 = Haste (m4) 2 (m1) 4 

m6 = Haste (Objeto 1) 6 (Objeto 3) 8

peso::Mobile->Int
peso (Objeto pes) = pes
peso (Haste m1 d1 m2 d2) = peso m1 + peso m2

equilibrio::Mobile->Bool
equilibrio (Objeto pes) = True
equilibrio (Haste m1 d1 m2 d2) = equilibrio m1 && equilibrio m2 && (peso m1) * d1 == (peso m2) * d2 

{-5-}
data Ponto = Pt Float Float
type Regiao = Ponto -> Bool

p1 = Pt 0 0
p2 = Pt 10 10
p3 = Pt 5 5
r1 = retangulo p1 p2

retangulo::Ponto->Ponto->Regiao
retangulo (Pt x1 y1) (Pt x2 y2) = (\(Pt x y) -> x >= x1 && x <= x2 && y >= y1 && y <= y2)

type Raio = Float

--circ = circulo p1 p2

circulo::Ponto->Raio->Regiao
circulo (Pt x1 y1) r = (\(Pt x y) -> x1>=(-r) && x2 <=r && y1>=(-r) && y2 <=r )

--------------------------------

type Vert = Int
type Grafo = ([Vert],[(Vert,Vert)])

{-
caminho::Grafo->[Vert]->Bool
caminho _ [] = True
caminho _ [x] = True
caminho (x_ (x:xs) = if elem(x)
-}