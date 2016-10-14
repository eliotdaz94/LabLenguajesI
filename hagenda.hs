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
toString = (concat.pixels)

stack :: [Picture] -> Picture
stack = foldl1 above 

spread :: [Picture] -> Picture
spread = foldl1 beside

row :: String -> Picture
row = spread.(map pixel)

blank :: (Height,Width) -> Picture
blank = uncurry blanko

blanko :: Height -> Width -> Picture
blanko h w = stack (map row (replicate h (replicate w ' ')))

--A estas 2 de aca les falta point free, no tengo ni idea de como hacerlas
stackWith :: Height -> [Picture] -> Picture
stackWith h ps = stack ((head ps) : (map (above (blank (h,width (head ps)))) (tail ps)))

spreadWith :: Width -> [Picture] -> Picture
spreadWith w ps = spread ((head ps) : (map (beside (blank (height (head ps) ,w))) (tail ps)))
--

tile :: [[Picture]] -> Picture
tile = stack.(map spread)

tileWith :: (Height, Width) -> [[Picture]] -> Picture
tileWith = uncurry tileWa

tileWa :: Height -> Width -> [[Picture]] -> Picture
tileWa h w pss = stackWith h (map (spreadWith w) pss)



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
jan1 y = toEnum (mod (sum (concatMap mlengths [1..y-1])) 7)

mtotals :: Year -> [Int]
mtotals y = scanl (+) (fromEnum (jan1 y)) (mlengths y)

fstdays :: Year -> [DayName]
fstdays y = map toEnum (map (`mod` 7) (mtotals y))

fstday :: Month -> Year -> DayName
fstday m y = (fstdays y) !! (fromEnum m)

day :: Day -> Month -> Year -> DayName
day d m y = toEnum (mod (fromEnum (fstday m y) + (d-1)) 7)
