--4
type Ponto = (Float, Float)
										
z = (1.0 , 2.0)
x = (3.0 , 4.0)

--a
dist p(x,y) q(w,z) = sqrt( (x-w)^2 + (y-z)^2 )

--b
comprimento [] = 0
comprimento [(x,y)] = 0
comprimento ((x,y):xs) = dist x (head xs) + comprimento xs

