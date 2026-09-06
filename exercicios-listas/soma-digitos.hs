somaDigitos::Int->Int
somaDigitos a = if a < 10 then a else (mod a 10) + somaDigitos (div a 10)
