{- |La función 'isPrefix' determina si el string p es prefijo del string s. -}
isPrefix :: String -- ^ String p.
		 -> String -- ^ String s.
		 -> Bool   -- ^ Booleano resultante.
isPrefix "" _          = True
isPrefix _ ""          = False
isPrefix (p:ps) (s:ss) = p == s && isPrefix ps ss

{- |La función 'removePrefix' calcula el resultado de remover el prefijo p del 
	string s. -}
removePrefix :: String -- ^ String p. 
			 -> String -- ^ String s.
			 -> String -- ^ String resultante de remover el prefijo p de s.
removePrefix p s = if isPrefix p s 
				   then drop (length p) s
				   else s

{- |La función 'suffixes' calcula todos los sufijos posibles de s en orden 
	descendiente de longitud. No se considera como sufijo el tipo base de 'a',
	por ejemplo '""' para a :: Char. -}
suffixes :: [a]   -- ^ Lista de tipo 'a' cuyos sufijos se calcularán. 
		 -> [[a]] -- ^ Lista resultante que contiene los sufijos posibles de s. 
suffixes [] = []
suffixes s  = s : suffixes (tail s)

{- |La función 'isSubstring' determine si s1 es una subcadena de s2. -}
isSubstring :: String -- ^ String s1. 
			-> String -- ^ String s2.
			-> Bool   -- ^ Booleano resultante de determinar si s1 es subcadena de s2.
isSubstring s1 s2 = foldl (||) False (map (isPrefix s1) (suffixes s2)) 

buscarIndices :: (Enum t, Num t) => (a -> Bool) -> [a] -> [t]
buscarIndices p x = [y | (x,y) <- zip x [0..] , p x]

{- |La función 'findSubstrings' produce los índices de todas las ocurrencias de s1
	en s2 usando búsqueda directa inocente. -}
findSubstrings :: String -- ^ String s1.
			   -> String -- ^ String s2.
			   -> [Int]  -- ^ Lista resultante que contiene los indices de las 
			   			 --	^ ocurrencias de s1 en s2.
findSubstrings s1 s2 = (buscarIndices (==True)) (map (isPrefix s1) (suffixes s2))

{- |Tipo de datos que modela un árbol de sufijos. Cada Node interno puede tener una 
	cantidad arbitraria de subárboles y cada subárbol está etiquetado con la cadena 
	correspondiente. -}
data SuffixTree = Leaf Int -- ^ Constructor del árbol de sufijos que representa una 
						   -- ^ hoja, donde se almacena el índice de un sufijo de s.
				| Node [(String,SuffixTree)] -- ^ Constructor del árbol de sufijos
											 -- ^ que representa un nodo.
				deriving (Eq,Ord,Show) 	

{- |La función 'getIndices' produce los valores almacenados en las hojas de un árbol
 	de sufijos, sin orden específico alguno. -}
getIndices :: SuffixTree -- ^ Árbol de sufijos.  
		   -> [Int] 	 -- ^ Lista resultante que contiene los valores de las hojas
		   				 -- ^ del árbol de sufijos.
getIndices (Leaf a) = [a]
getIndices (Node x) = concatMap (\(y,z) -> (getIndices z)) x

{- |La función 'findSubstrings'' implanta el método de búsqueda en árboles de sufijos 
	descrito en el enunciado del proyecto, para retornar los índices de todas las 
	ocurrencias de s en t. -}
findSubstrings' :: String     -- ^ String s.
				-> SuffixTree -- ^ Árbol de sufijos t.
				-> [Int]      -- ^ Lista resultante que contiene los índices de todas
							  -- ^ las ocurrencias de s en t.
findSubstrings' s (Leaf _) = []
findSubstrings' s (Node x)
	| isPrefix s a = getIndices b
	| isPrefix a s = findSubstrings' (removePrefix a s) b
	| otherwise    = findSubstrings' s c
	where 
		a = ((fst.head) x)
		b = ((snd.head) x)
		c = (Node (tail x))

{- |La función 'insert' produce el árbol de sufijos resultante de insertar el sufijo s
	con índice i en el árbol t. -}
insert :: (String,Int) -- ^ Tupla que contiene el sufijo s y el indice i, a insertar
					   -- ^ en el árbol de sufijos t. 
	   -> SuffixTree   -- ^ Árbol de sufijos t.
	   -> SuffixTree   -- ^ Árbol de sufijos resultante de insertar el sufijo s
					   -- ^ con índice i en el árbol t.
insert (s,i) (Leaf a)  = (Leaf a)
insert (s,i) (Node xs) = if isPrefixAux s (map fst xs)
							then Node (map (insrtAux (s,i)) xs)
							else Node ((s, Leaf i) : xs)

{- |La función auxiliar 'isPrefixAux' determina si s es prefijo de alguna cadena
	perteneciente a lista xs. -}
isPrefixAux :: String   -- ^ String s.
			-> [String] -- ^ Lista de Strings xs.
			-> Bool     -- ^ Booleano resultante de determinar si s es prefijo de 
						-- ^ alguna cadena perteneciente a lista xs. 
isPrefixAux "" xs = False 
isPrefixAux s xs  = foldl (||) False (map (isPrefix [head s]) xs)

{- |La función auxiliar 'insrtAux' inserta el sufijo p con índice i en el árbol de
	sufijos t, modificando la etiqueta de t, s. -}
insrtAux :: (String,Int)        -- ^
		 -> (String,SuffixTree) -- ^ 
		 -> (String,SuffixTree) -- ^
insrtAux (p,i) (s,t) = if newS /= ""
						then (newS, Node[((removePrefix newS s),t),
										 ((removePrefix newS p), Leaf i)])
						else (s,t)
							where newS = removeAux p s

{- |La función auxiliar 'removeAux' calcula el resultado de remover el prefijo 
	común más largo de las cadenas p y s. -}
removeAux :: String -- ^ String p.
		  -> String -- ^ String s.
		  -> String -- ^ String resultante de remover el prefijo común más largo de 
		  			-- ^ p y s.
removeAux "" s = ""
removeAux p "" = ""
removeAux p s  = if (head p) == (head s)
					then head p : (removeAux (tail p) (tail s))
					else ""

{- |La función 'buildTree' construye el árbol de sufijos a partir de s.-}
buildTree :: String     -- ^ String s.
		  -> SuffixTree -- ^ Árbol de sufijos construido a partir de s.
buildTree s = foldl (flip insert) (Node []) (reverse (fst (foldl entuplar ([],0) (suffixes s))))
				where entuplar (xs,n) s = ((s,n) : xs, n + 1)