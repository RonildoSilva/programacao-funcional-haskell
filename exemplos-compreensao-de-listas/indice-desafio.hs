indice :: Int -> [Int] -> [Int]

indice n xs = [x|(x,y)<-zip zip[0..] xs, y==n]
