--3
data Ponto = Pt Float Float
type Regiao = Ponto -> Bool

--a
dist' p(x y) q(z w) = sqrt( (x-z)^2 + (y-w)^2 )  
retang P(x1 y1) Q(x2 y2) = (\(Pt x y) -> if (x>x1 && x<x2) && (y>y1 && y <y2) then True else False ) 
circ P(x y) r = (\(Pt z w) -> if dist' (x y) (z w)<r then True else False)

--b
--uniao r r' = if (\(Pt x y) r) || (\(Pt x y) r') then True else False

