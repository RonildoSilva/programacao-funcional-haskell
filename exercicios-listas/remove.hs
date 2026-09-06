areTheySame :: Int -> Int-> [Int]

areTheySame x y | x == y = []
                | otherwise = [y]

removeItem :: Int -> [Int] -> [Int]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
