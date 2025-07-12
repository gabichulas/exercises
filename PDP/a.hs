-- Función para calcular el volumen de una esfera
-- Sintaxis: nombreFuncion :: TipoParametro -> TipoRetorno
volumenEsfera :: Double -> Double
volumenEsfera radio = (4/3) * pi * radio^3

sumaCoins :: Fractional a => a -> a -> a -> a -> a -> a
sumaCoins a b c d e = a*0.01 + b*0.05 + c*0.1 + d*0.5 + e

sumaTupla :: Num a => [a] -> [a]
sumaTupla [a,b,c] = [a+1,b+1,c+1]

power :: Num a => a -> a
power a = a*a

equal3 x y z = x == y && y==z 

divisores a = [x | x <- [-a..a], x /= 0, a `mod` x == 0]

primo x = length (divisores x) == 4

primos n = [x | x <- [-n..n], x /= 0, primo x]

tomar :: Int -> [a] -> [a]
tomar 0 _ = []
tomar _ [] = []
tomar n (x:xs) = x : tomar (n-1) xs

tomarMientras :: (a -> Bool) -> [a] -> [a]
tomarMientras _ [] = []
tomarMientras p (x:xs)
  | p x       = x : tomarMientras p xs
  | otherwise = []


nIndex :: [a] -> Int -> Maybe a
nIndex [] _ = Nothing
nIndex (x:_) 0 = Just x
nIndex (_:xs) n = nIndex xs (n-1)

_elem _ [] = False
_elem e (x:_) | e==x = True
_elem e (_:xs) = _elem e xs

-- Definir el tipo de dato para números complejos
data Complejo = Complejo Double Double deriving (Show, Eq)

-- Función para crear un número complejo
complejo :: Double -> Double -> Complejo
complejo real imaginario = Complejo real imaginario

-- Suma de números complejos
sumaComplejo :: Complejo -> Complejo -> Complejo
sumaComplejo (Complejo r1 i1) (Complejo r2 i2) = Complejo (r1 + r2) (i1 + i2)

-- Multiplicación de números complejos
-- (a + bi) * (c + di) = (ac - bd) + (ad + bc)i
multiplicacionComplejo :: Complejo -> Complejo -> Complejo
multiplicacionComplejo (Complejo r1 i1) (Complejo r2 i2) = 
  Complejo (r1 * r2 - i1 * i2) (r1 * i2 + i1 * r2)

-- Otras operaciones útiles
parteReal :: Complejo -> Double
parteReal (Complejo r _) = r

parteImaginaria :: Complejo -> Double
parteImaginaria (Complejo _ i) = i


ocurrencias _ [] = 0
ocurrencias e (x:xs) | e == x = 1 + ocurrencias e xs
                     | otherwise = ocurrencias e xs 



enterosEntre a b = [a..b]

mapSucesor = map (+1)

positivos = filter (>0)

reversa' [] = []
reversa' (x:xs) = reversa' xs ++ [x]

sumPar [a,b] = a+b

sumaPar = map sumPar

zipMaximos a b = zipWith max a b


data Date = Date {dia :: Int, mes :: Int, ano :: Int} deriving (Show, Eq, Ord)

data Persona = Persona {nombre :: String, apellido :: String, fecha :: Date} deriving (Show, Eq)

fechaMenor :: Date -> Date -> Bool
fechaMenor (Date d1 m1 a1) (Date d2 m2 a2)
  | a1 < a2 = True
  | a1 > a2 = False
  | m1 < m2 = True
  | m1 > m2 = False
  | d1 < d2 = True
  | otherwise = False



extractJovenes :: [Persona] -> Date -> [Persona]
extractJovenes lista fechaLimite = tomarMientras (\p -> fechaMenor (fecha p) fechaLimite) lista


sumaList [] = 0
sumaList (x:xs) = x + sumaList xs

trueList :: [Bool] -> Bool
trueList [] = False
trueList (True:_) = True
trueList (False:xs) = trueList xs

todos [] = False
todos (False:_) = False
todos (True:xs) = todos xs



findList _ [] = Nothing
findList a (x:xs) 
  | a == [x]    = Just 0
  | otherwise = case findList a xs of
                  Nothing -> Nothing
                  Just n  -> Just (n + 1)



