isPrefix :: String -> String -> Bool
isPrefix "" _ = True
isPrefix _ "" = False
--isPrefix p s = p  == take (length p) s
isPrefix (p:ps) (s:ss) = p == s && isPrefix ps ss


removePrefix :: String -> String -> String
removePrefix p s = if isPrefix p s 
				   then drop (length p) s
				   else s

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes s = s : suffixes (tail s)

isSubstring :: String -> String -> Bool
isSubstring s1 s2 = foldl (||) False (map (isPrefix s1) (suffixes s2)) 

buscarIndices p x = [y | (x,y) <- zip x [0..] , p x]

findSubstrings :: String -> String -> [Int]
findSubstrings s1 s2 = (buscarIndices (==True)) (map (isPrefix s1) (suffixes s2))


data SuffixTree = Leaf Int
				| Node [(String,SuffixTree)]
				deriving (Eq,Ord,Show)

t1 :: SuffixTree
t1 = Node [("banana", Leaf 0),
		   ("a", Node [("na", Node [("na", Leaf 1),
									("", Leaf 3)]),
					   ("", Leaf 5)]),
		   ("na", Node [("na", Leaf 2),
						("", Leaf 4)])]

t2 :: SuffixTree
t2 = Leaf 2

pruebita (Node x) = Node( (tail x))

getIndices :: SuffixTree -> [Int]
getIndices (Leaf a) = [a]
getIndices (Node x) = concatMap (\(y,z) -> (getIndices z)) x

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' s (Leaf _) = []
findSubstrings' s (Node x)
	| isPrefix s a = getIndices b
	| isPrefix a s = findSubstrings' (removePrefix a s) b
	| otherwise    = findSubstrings' s c
	where 
		a = ((fst.head) x)
		b = ((snd.head) x)
		c = (Node (tail x))