import Data.Char (toUpper, toLower)

pares n = [x | x <- [0..n], even x]


paresSi n m = [x | x <- [0..n], even x, x > m]

generarNumeros n = [n | _  <- [1..n]]

copias xs = concat [generarNumeros x | x <- xs, x > 0]

contarPares (a,b,c) = length [x | x <- [a,b,c], even x]

esPrimo :: Int -> Bool
esPrimo n 
  | n < 2     = False
  | n == 2    = True
  | even n    = False
  | otherwise = not (any (\x -> n `mod` x == 0) [3,5..sqrt' n])
  where sqrt' = floor . sqrt . fromIntegral


primos n = [x | x <- [1..n], esPrimo x]


posiciones :: String -> Char -> [Int]
posiciones xs y = [i | (i, c) <- zip [0..] xs, c == y]



mayusOrNot :: String -> String
mayusOrNot [] = []  -- Caso para string vacío
mayusOrNot (x:xs) 
  | length (x:xs) >= 4 = toUpper x : map toLower xs
  | otherwise = map toLower (x:xs)  -- "otherwise" bien escrito

mayusWord :: String -> String
mayusWord (x:xs) = toUpper x : map toLower xs

titulo :: [String] -> [String]
titulo (p:ps) = mayusWord p : [mayusOrNot x | x <- ps]