putStrl::String->IO()
putStrl [] = putChar '\n'
putStrl (c:cs) = putChar c >> putStrl cs

putStrl (c:cs) = do{
	putChar c;
	putStrl cs;
}


getLine::IO String
getLine=getChar >> (\c -> if c=='\n'then return [])
			else getLine >> = (\l -> return (c:l))

getLine = do{
	c<-getChar
	if c == '\n' return[]
	else l <-getLine
	return(c:l);
}
-----------------------------------------------
repeat'::(a->Bool)->Io a-> IO(a)
repeat p io = do{
		c<-io
		if p c 
}

--getLine = repeat (\='\n') getChar
-----------------------------------------------

accumulate::[Io a]->[Io a]
--accumulate[getChar,getChar,getChar] -> Io[Char]
accumulate (x:xs) = do {
	c<-x
	l<-accumulate xs
	return(c:l)
}

accumulate (x:xs) = x>>= (\c->accumulate xs >>= (\l->return (c:l)))

sequencia::[Io ()]->Io()
--	sequencia [putChar 'a',putChar 'b']

