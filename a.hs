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