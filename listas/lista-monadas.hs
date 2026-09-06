liftM :: Monad m => (a->b) -> m a -> m b
liftM f m1 = do {
	x1 <- m1;
	return (f x1);
}

-- a
mapM' :: Monad m => (a-> m b) -> [a] -> m [b]
mapM' f [] = return []
mapM' f (x:xs) = do {
	y <- f x;
	ys <- mapM' f xs;
	return (y:ys);
}

-- b
mapM_' :: Monad m => (a-> m b) -> [a] -> m ()
mapM_' f [] = return ()
mapM_' f (x:xs) = do {
	f x;
	mapM_' f xs
}

-- 2
zip' :: ([a], [b]) -> [(a,b)]
zip' ( [] , _ ) = []
zip' ( _ , [] ) = []
zip' ((x:xs), (y:ys)) = (x, y) : zip' (xs,ys) 

unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([],[])
unzip' ((x,y) : zs) = let (xs, ys) = unzip zs in (x:xs, y:ys)

nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) = x: filter (x/=) (nub' xs)

-- 3 -- ------------
addPWRec [] = []
--addPWRec (x:[]) = [x]
addPWRec (x:xs) = (sum x + sum ((!!) xs (length xs -1))) : addPWRec (init xs)



type Passaros = Int
type Barra = (Passaros , Passaros)

pousoEsq :: Passaros -> Barra -> Maybe Barra
pousoEsq n (esq, dir) | abs ((esq +n) - dir) < 4 = Just (esq + n , dir)
					  | otherwise = Nothing


pousoDir :: Passaros -> Barra -> Maybe Barra
pousoDir n (esq, dir) | abs (esq  - (dir+n)) < 4 = Just (esq , dir+n)
					  | otherwise = Nothing



rotina1 :: Maybe Barra
rotina1 = do {
	b1 <- pousoEsq 1 (0,0);
	b2 <- pousoDir 4 b1;
	b3 <- pousoEsq 2 b2;
	return b3;
}

data Pos = Esq | Dir deriving (Eq , Show)
type Pouso  = (Pos, Passaros)

b1 = (0,0) :: Barra
p1 = (Esq, 2) :: Pouso
p2 = (Dir, 4) :: Pouso
sq = [p1,p2]


-- bugada ainda... :/
--rotina2 :: Barra -> [Pouso] -> Maybe Barra
--rotina2 b [] = Nothing
--rotina2 b ( (pos, ps) :xs) = do {
--	if pos == Dir then nb <- pousoDir ps b; 
--	else nb <- pousoEsq ps b; 
--	rotina2 nb xs;
--}




