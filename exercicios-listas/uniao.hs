pertence a [] = False
pertence a (x:xs) = if (a == x) then True
		else pertence a xs

uniaoR [] l = l
uniaoR (x:xz) l = if pertence x l then uniaoR xz l
		else x: uniaoR xz l
