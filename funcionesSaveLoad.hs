data Evento = Evento {
				year 		:: Year,
				month 		:: Month,
				eday 		:: Day,
				nth 		:: Int,
				description :: String
			}
		deriving (Show,Read)

{- |La función auxiliar 'compareEvent' permite comparar dos eventos e1 y e2 para
	determinar si e1 es más reciente que e2. -}
compareEvent :: Evento   -- ^ Evento e1.
			 -> Evento   -- ^ Evento e2.
			 -> Ordering -- ^ Orden resultante de comparar los eventos e1 y e2.
compareEvent e1 e2 = if (compareEventYear e1 e2) == EQ then
						if (compareEventMonth e1 e2) == EQ then
							if (compareEventDay e1 e2 == EQ) then
								compareEventNth e1 e2
							else compareEventDay e1 e2
						else compareEventMonth e1 e2
					 else compareEventYear e1 e2

{- |La función auxiliar 'compareEventYear' permite comparar dos eventos e1 y e2 
	para determinar si el año de e1 es más reciente que el año de e2. -}
compareEventYear :: Evento   -- ^ Evento e1.
			 	 -> Evento   -- ^ Evento e2.
			 	 -> Ordering -- ^ Orden resultante de comparar los eventos e1 y e2
			 	 			 -- ^ por año.
compareEventYear e1 e2
	| year e1 > year e2  = GT
	| year e1 < year e2  = LT
	| year e1 == year e2 = EQ

{- |La función auxiliar 'compareEventMonth' permite comparar dos eventos e1 y e2 
	para determinar si el mes de e1 es más reciente que el mes de e2. -}
compareEventMonth :: Evento   -- ^ Evento e1.
			 	  -> Evento   -- ^ Evento e2.
			 	  -> Ordering -- ^ Orden resultante de comparar los eventos e1 y e2
			 	 			  -- ^ por mes.
compareEventMonth e1 e2
	| month e1 > month e2  = GT
	| month e1 < month e2  = LT
	| month e1 == month e2 = EQ

{- |La función auxiliar 'compareEventDay' permite comparar dos eventos e1 y e2 
	para determinar si el día de e1 es más reciente que el día de e2. -}
compareEventDay :: Evento   -- ^ Evento e1.
			 	-> Evento   -- ^ Evento e2.
			 	-> Ordering -- ^ Orden resultante de comparar los eventos e1 y e2
			 	 			-- ^ por día.
compareEventDay e1 e2
	| eday e1 > eday e2  = GT
	| eday e1 < eday e2  = LT
	| eday e1 == eday e2 = EQ

{- |La función auxiliar 'compareEventDay' permite comparar dos eventos e1 y e2 
	para determinar si el número de e1 es más reciente que el número de e2. -}
compareEventNth :: Evento   -- ^ Evento e1.
			 	-> Evento   -- ^ Evento e2.
			 	-> Ordering -- ^ Orden resultante de comparar los eventos e1 y e2
			 	 			-- ^ por número.
compareEventNth e1 e2
	| nth e1 > nth e2  = GT
	| nth e1 < nth e2  = LT
	| nth e1 == nth e2 = EQ 		

{- |La función 'loadEvents' permite cargar los eventos registrados en el archivo 
	de nombre FilePath. -}
loadEvents :: FilePath    -- ^ Nombre del archivo.
		   -> IO [Evento] -- ^ Acción resultante de cargar una lista de eventos.
loadEvents pathName = do
	content <- readFile pathName >>= return . lines
	let eventos = sortBy compareEvent (map (read::String->Evento) content)
	return eventos

{- |La función 'saveEvents' permite guardar los eventos que se encuentran en 
    una lista, en el archivo de nombre FilePath. -}
saveEvents :: FilePath -- ^ Nombre del archivo.  
		   -> [Evento] -- ^ Lista de eventos a guardar.
		   -> IO ()    -- ^ Acción resultante de guardar la lista de eventos.
saveEvents pathName eventos = do
	let content = unlines (map show (sortBy compareEvent eventos)) 
	writeFile pathName content

{- |La función 'eventsOnMonth' produca una lista que contiene los días del mes m 
	que tienen un evento registrado en es. -}
eventsOnMonth :: [Evento] -- ^ Lista de eventos es.  
			  -> Month    -- ^ Mes m.
			  -> [Day]    -- ^ Lista que contiene los días resultantes.
eventsOnMonth es m = fst (foldl monthAux ([],m) es)

{- |La función auxiliar 'monthAux' dada una tupla qur contiene una lista de dias
	y un mes, y un evento, determinar los dias de dicho mes que contienen eventos
	sin duplicados. -}
monthAux :: ([Day],Month) -- ^ Tupla que contiene la lista dias y el mes. 
		 -> Evento        -- ^ Evento.
		 -> ([Day],Month) -- ^ Tupla resultante.
monthAux (xs,m) e = if m == (month e) 
						then
							if elem (eday e) xs 
								then (xs,m)
								else ((eday e) : xs,m)
						else (xs,m)