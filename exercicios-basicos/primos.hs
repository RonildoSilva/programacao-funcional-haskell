primos :: Int -> Bool
primos a = if length [ x | x <- [1..a], mod a x == 0] == 2 then True else False