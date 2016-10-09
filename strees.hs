import Data.List
import Data.Maybe

isPrefix :: String -> String -> Bool
isPrefix p s = isPrefixOf p s

--Suponiendo que cuando el prefijo p no se encuentra en s,
--retorna s.
removePrefix :: String -> String -> String
removePrefix p s = fromMaybe s (stripPrefix p s)

--Cuales son los sufijos de una lista vacia []???
suffixes :: [a] -> [[a]]
suffixes s = init (tails s)

suffixes2 :: [a] -> [[a]]
suffixes2 [] = []
suffixes2 xs = xs : (suffixes2 . tail) xs

--Utilizando isPrefix suffixes
isSubstring :: String -> String -> Bool
isSubstring s1 s2 = foldl (||) False (map (isPrefix s1) (suffixes s2))
