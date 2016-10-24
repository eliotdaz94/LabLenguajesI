{- |La función 'unzipR' transforma una lista de tuplas en una lista con las primeras 
	componentes y una lista con las segundas componentes, de dichas tuplas. Se 
	implemente utilizando recursión directa. -}
unzipR :: [(a,b)]   -- ^ Lista con tuplas.
	   -> ([a],[b]) -- ^ Tupla resultante con la lista de primeras componentes y la
	   				-- ^ lista de segundas componentes.
unzipR [] 	  = ([],[])
unzipR (x:xs) = (fst x : fst r, snd x : snd r)
	where r = unzipR xs

{- |La función 'unzipR' transforma una lista de tuplas en una lista con las primeras 
	componentes y una lista con las segundas componentes, de dichas tuplas. Se 
	implemente utilizando un foldr. -}
unzipF :: [(a,b)]   -- ^ Lista con tuplas.
	   -> ([a],[b]) -- ^ Tupla resultante con la lista de primeras componentes y la
	   				-- ^ lista de segundas componentes.
unzipF xs = foldr (\t1 t2 -> (fst t1 : fst t2, snd t1 : snd t2)) ([],[]) xs

{- |La función 'unzipR' transforma una lista de tuplas en una lista con las primeras 
	componentes y una lista con las segundas componentes, de dichas tuplas. Se 
	implemente utilizando un map. -}
unzipM :: [(a,b)]   -- ^ Lista con tuplas.
	   -> ([a],[b]) -- ^ Tupla resultante con la lista de primeras componentes y la
	   				-- ^ lista de segundas componentes.
unzipM xs = (map fst xs, map snd xs)   

{- |La función 'unzipR' transforma una lista de tuplas en una lista con las primeras 
	componentes y una lista con las segundas componentes, de dichas tuplas. Se 
	implemente utilizando listas por comprensión. -}
unzipL :: [(a,b)]   -- ^ Lista con tuplas.
	   -> ([a],[b]) -- ^ Tupla resultante con la lista de primeras componentes y la
	   				-- ^ lista de segundas componentes.
unzipL xs = ([fst x | x <- xs], [snd x | x <- xs])