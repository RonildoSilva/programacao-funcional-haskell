mdc :: Int -> Int -> Int
mdc a b = if b == 0 then a else mdc b (mod a b)
