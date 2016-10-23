insert :: (String,Int) -> SuffixTree -> SuffixTree
insert (s,i) (Leaf n)      = Node [("" , Leaf n) , (s , Leaf i)] 
insert (s,i) (Node xs)     = Node (auxInsrt (s,i) xs) 

auxInsrt (s,i) []     = [(s, Leaf i)]
auxInsrt (s,i) (x:xs) = if p /= "" && newS /= ""
							then (newS,insert () ())
							then (p,(insert ((removePrefix p s),i) t)) : xs
							else  x : (auxInsrt (s,i) xs)
							where
								newS = h (preffixes s) p
								p    = fst x
								t    = snd x

preffixes [] = []
preffixes s = s : preffixes (init s)

h [] s     = "" 
h (p:ps) s = if isPrefix p s 
				then p
				else h ps s