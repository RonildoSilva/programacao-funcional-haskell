--a
fatAcc n = scanl (*) 1 [2..n] 
--b 
fatorial n = last (fatAcc n)

{-1-}
fact 1 = 1
fact n = n * fact(n-1)

fat = scanl (*) 1 [1..]

{-tail $take 10 fat-}

{-2-}
dotProd (a:as) (b:bs) = sum $zipWith (+) (a:as) (b:bs)


{-3-}
pitagoricos 0 = []
pitagoricos x = [(a,b,c) | a<-[1..x], b<-[1..x], c<-[1..x],(a^2)+(b^2)==(c^2)]

{-4-}

{-4 a-}
numerador n = [4 * (-1)^i | i <-[0..n]]
{-take 11 $ cycle [4,-4]-}

{-4 b-}
denominadores = [1,3..]
{-take 10 denominadores-}

{-4 c-}
num = cycle[4,-4]
den = [a*(a+1)*(a+2) | a <- [2,4..]]

{-4 d-}
calcPi n = sum $ take n pi2

pi2 = zipWith (/) (cycle [4, -4]) denominadores

{-5 a-}
calcPi2 n = 3 + sum (take(n-1) $ zipWith (/) num den)

{-6 a-}
binom n k = (product [1..n] `div` (product [1..k] * product [1..(n-k)]))

{-6 b-}
binom2 n k = (product [(k+1)..n] `div` (product [1..k] * product [1..(n-k)]))

{-6 c-}
pascal n = [[binom i j | j <- [0..i]] | i <- [0..n]]

{-6 d-}
pascal2 = [[binom i j | j <- [0..i]] | i <- [0..]]

{-6 e incompleto-}
binom3 n 0 = 1
binom3 n k | k == n  = 1
binom3 n k |otherwise = (binom3 (n-1) (k-1) + (binom3 (n-1) k))

--pascal3 = [[binom3 i j] | j <-[0..j] |]

{-7-}
--horner :: (Num a) => a -> [a] -> a
--horner x = foldr (\a b -> a + b*x) 0
 
horner [] z = 0
horner (c:cs) z = c + z *(horner cs z)

--main = print $ horner 3 [-19, 7, -4, 6]

{-8-}
tri 1 = 1
tri n = n+tri (n-1)
triangular = map tri [1..]

triang = scanl (+) 0 [1..]

--s(Ô).(Ô)2

{-9-}
fib n = fib' n 1 2
	where
	fib' 0 a _ = a
	fib' 1 _ b = b
	fib' n a b = fib' (n-1) b (a+b)

elemento n [] = False
elemento n (x:xs) = if n == x then True 
		else elemento n xs

el x xs = foldr (\y ys -> x==y || ys) False xs
--el x xs = foldr (\y xs -> x==y) False xs
remdup xs = foldr(\x xs -> if elem x xs then xs else (x:xs)) [] xs