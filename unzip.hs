unzipR [] 	  = ([],[])
unzipR (x:xs) = (fst x : fst r, snd x : snd r)
	where r = unzipR xs

unzipF xs = foldr f ([],[]) xs
	where f t1 t2 = (fst t1 : fst t2, snd t1 : snd t2)

unzipF2 xs = foldr (\t1 t2 -> (fst t1 : fst t2, snd t1 : snd t2)) ([],[]) xs

unzipM xs = (map fst xs, map snd xs)   

unzipL xs = ([fst x | x <- xs], [snd x | x <- xs])