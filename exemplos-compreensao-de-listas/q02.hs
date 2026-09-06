pert x xs = if length [y|y<-xs, y == x] >= 1 then True else False
