data Evento = Evento {
				year 		:: Year,
				month 		:: Month,
				eday 		:: Day,
				nth 		:: Int,
				description :: String
			}
		deriving (Show,Read)

compareEvent e1 e2 = if (compareEventYear e1 e2) == EQ then
						if (compareEventMonth e1 e2) == EQ then
							if (compareEventDay e1 e2 == EQ) then
								compareEventNth e1 e2
							else compareEventDay e1 e2
						else compareEventMonth e1 e2
					 else compareEventYear e1 e2

compareEventYear :: Evento -> Evento -> Ordering
compareEventYear e1 e2
	| year e1 > year e2  = GT
	| year e1 < year e2  = LT
	| year e1 == year e2 = EQ

compareEventMonth :: Evento -> Evento -> Ordering
compareEventMonth e1 e2
	| month e1 > month e2  = GT
	| month e1 < month e2  = LT
	| month e1 == month e2 = EQ

compareEventDay :: Evento -> Evento -> Ordering
compareEventDay e1 e2
	| eday e1 > eday e2  = GT
	| eday e1 < eday e2  = LT
	| eday e1 == eday e2 = EQ

compareEventNth :: Evento -> Evento -> Ordering
compareEventNth e1 e2
	| nth e1 > nth e2  = GT
	| nth e1 < nth e2  = LT
	| nth e1 == nth e2 = EQ 		

loadEvents :: FilePath -> IO [Evento]
loadEvents pathName = do
	content <- readFile pathName >>= return . lines
	let eventos = sortBy compareEvent (map (read::String->Evento) content)
	return eventos

saveEvents :: FilePath -> [Evento] -> IO ()
saveEvents pathName eventos = do
	let content = unlines (map show (sortBy compareEvent eventos)) 
	writeFile pathName content

eventsOnMonth :: [Evento] -> Month -> [Day]
eventsOnMonth es m = fst (foldl monthAux ([],m) es)

monthAux :: ([Day],Month) -> Evento -> ([Day],Month)
monthAux (xs,m) e = if m == (month e) 
						then
							if elem (eday e) xs 
								then (xs,m)
								else ((eday e) : xs,m)
						else (xs,m)