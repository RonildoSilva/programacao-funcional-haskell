--dobro :: Num a => a -> a
dobro x = 2*x
--quadruplo :: Num a => a -> a
quadruplo x = dobro ( dobro x)
--divisores :: Int -> [Int]
divisores n = [ x | x <- [1..n], mod n x == 0]
--divisoresComum :: Int -> Int -> [Int]
divisoresComum a b = [ x | x <- [1..a], mod a x == 0, mod b x == 0]
--menor2 :: Ord a => a -> a -> a
menor2 a b = if a <= b then a else b
