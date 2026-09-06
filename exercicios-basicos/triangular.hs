{-
triangularR 1 = 1
triangularR n = n + triangularR (n-1)
-}

tri 1 = 1
tri n = n+tri (n-1)
triangular = map tri [1..]
