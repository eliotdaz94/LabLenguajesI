type Height = Int
type Width  = Int


data Picture = Picture {
				height :: Height,
				width  :: Width,
				pixels :: [[Char]]
			} deriving (Show)


pixel :: Char -> Picture
pixel c = Picture 1 1 [[c]]

above :: Picture -> Picture -> Picture
above p0 p1 = if width p0 == width p1
			  then Picture (height p0 + height p1) (width p0) (pixels p0 ++ pixels p1)
			  else error "can't 'above' different widths"

beside :: Picture -> Picture -> Picture
beside p0 p1 = if height p0 == height p1
			   then Picture (height p0) (width p0 + width p1) (unirChars (pixels p0) (pixels p1))
			   else error "can't 'beside' different heights"


unirChars :: [[Char]] -> [[Char]] -> [[Char]]
unirChars [] [] = []
unirChars [] (y:ys) = (y:ys)
unirChars (x:xs) [] = (x:xs)
unirChars (x:xs) (y:ys) = (x++y) : unirChars xs ys 

toString :: Picture -> String
toString = (unlines.pixels)

stack :: [Picture] -> Picture
stack = foldl1 above 

spread :: [Picture] -> Picture
spread = foldl1 beside

row :: String -> Picture
row = spread.(map pixel)

blank :: (Height,Width) -> Picture
blank = (\(x,y) -> stack $ map row (replicate x (replicate y ' ')))

stackWith :: Height -> [Picture] -> Picture
stackWith = (\x y -> stack $ (head y) : (map (above (blank (x,width (head y)))) (tail y)))

spreadWith :: Width -> [Picture] -> Picture
spreadWith = (\x y -> spread $ (head y) : (map (beside (blank (height (head y) ,x))) (tail y)))

tile :: [[Picture]] -> Picture
tile = stack.(map spread)

tileWith :: (Height, Width) -> [[Picture]] -> Picture
tileWith = (\(x,y) z -> stackWith x $ map (spreadWith y) z)




x :: Picture
y :: Picture
z :: Picture

x = Picture 1 4 ["hola"]
y = Picture 1 4 ["como"]
z = Picture 1 4 ["esta"]


type Day   = Int
-- Suponga que est· entre 1 y 31
type Year  = Int
-- Suponga que es positivo
data Month = Enero
		   | Febrero
		   | Marzo
		   | Abril
		   | Mayo
		   | Junio
		   | Julio
		   | Agosto
		   | Septiembre
		   | Octubre
		   | Noviembre
		   | Diciembre
		   deriving (Show,Eq,Ord,Enum)

data DayName = Domingo
	  		 | Lunes
			 | Martes
			 | Miercoles
			 | Jueves
			 | Viernes
			 | Sabado
			 deriving (Show,Eq,Ord,Enum)


leap :: Year -> Bool
leap y = if(mod y 4 == 0 || mod y 400 == 0)
		 then True
		 else False

mlengths :: Year -> [Day]
mlengths a = [31,28 + b,31,30,31,30,31,31,30,31,30,31]
	where b = if(leap a)
			  then 1
			  else 0

jan1 :: Year -> DayName
jan1 y = toEnum $ mod (sum (concatMap mlengths [1..y-1])) 7

mtotals :: Year -> [Int]
mtotals y = scanl (+) (fromEnum (jan1 y)) (mlengths y)

fstdays :: Year -> [DayName]
fstdays y = map toEnum $ map (`mod` 7) (mtotals y) 

fstday :: Month -> Year -> DayName
fstday m y = (fstdays y) !! (fromEnum m)

day :: Day -> Month -> Year -> DayName
day d m y = toEnum $ mod (fromEnum (fstday m y) + (d-1)) 7

rjustify :: Int -> String -> String
rjustify n s = if(n > length s)
			   then concat (replicate (n - length s) " ") ++ s
			   else s

intersperse' :: a -> [a] -> [a]
intersperse' a [] = []
intersperse' a (x:xs) = x : (inter a xs)
	where
		inter a [] = []
		inter a (y:ys) = a : (intersperse' a (y:ys))  

dnames :: Picture
dnames = beside (pixel ' ') $ spreadWith 1 $ map (row.(take 2).show.cast) [0..6]
	where cast i = toEnum i :: DayName

banner :: Month -> Year -> Picture
banner m y = row $ rjustify (length (head (pixels dnames))) (show(m) ++ " " ++ show(y))

heading :: Month -> Year -> Picture
heading m y = banner m y `above` dnames