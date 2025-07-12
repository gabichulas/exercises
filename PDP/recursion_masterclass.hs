-- ====================================================================
-- MASTERCLASS: FUNCIONES RECURSIVAS EN HASKELL
-- ====================================================================

-- ====================================================================
-- 1. CONCEPTOS BÁSICOS
-- ====================================================================

{-
¿Qué es la recursión?
Una función es recursiva cuando se llama a sí misma para resolver 
un problema más pequeño del mismo tipo.

Componentes esenciales:
1. CASO BASE: Condición que detiene la recursión
2. CASO RECURSIVO: La función se llama a sí misma con parámetros más pequeños
3. PROGRESO: Cada llamada debe acercarse al caso base
-}

-- ====================================================================
-- 2. RECURSIÓN CON NÚMEROS
-- ====================================================================

-- Factorial: n! = n * (n-1) * (n-2) * ... * 1
factorial :: Int -> Int
factorial 0 = 1                    -- CASO BASE
factorial n = n * factorial (n-1)  -- CASO RECURSIVO

-- Fibonacci: F(n) = F(n-1) + F(n-2)
fibonacci :: Int -> Int
fibonacci 0 = 0                               -- CASO BASE 1
fibonacci 1 = 1                               -- CASO BASE 2
fibonacci n = fibonacci (n-1) + fibonacci (n-2)  -- CASO RECURSIVO

-- Potencia: x^n
potencia :: Int -> Int -> Int
potencia _ 0 = 1                              -- CASO BASE
potencia x n = x * potencia x (n-1)          -- CASO RECURSIVO

-- Suma de números de 1 a n
sumaHasta :: Int -> Int
sumaHasta 0 = 0                               -- CASO BASE
sumaHasta n = n + sumaHasta (n-1)            -- CASO RECURSIVO

-- ====================================================================
-- 3. RECURSIÓN CON LISTAS
-- ====================================================================

-- Longitud de una lista
longitud :: [a] -> Int
longitud [] = 0                               -- CASO BASE
longitud (_:xs) = 1 + longitud xs             -- CASO RECURSIVO

-- Suma de elementos de una lista
sumaLista :: Num a => [a] -> a
sumaLista [] = 0                              -- CASO BASE
sumaLista (x:xs) = x + sumaLista xs           -- CASO RECURSIVO

-- Máximo elemento de una lista
maximoLista :: Ord a => [a] -> a
maximoLista [x] = x                           -- CASO BASE
maximoLista (x:xs) = max x (maximoLista xs)   -- CASO RECURSIVO

-- Reversa de una lista
reversa :: [a] -> [a]
reversa [] = []                               -- CASO BASE
reversa (x:xs) = reversa xs ++ [x]            -- CASO RECURSIVO

-- ====================================================================
-- 4. RECURSIÓN CON PATTERN MATCHING AVANZADO
-- ====================================================================

-- Tomar n elementos de una lista
tomar :: Int -> [a] -> [a]
tomar 0 _ = []                                -- CASO BASE 1
tomar _ [] = []                               -- CASO BASE 2
tomar n (x:xs) = x : tomar (n-1) xs          -- CASO RECURSIVO

-- Eliminar n elementos de una lista
eliminar :: Int -> [a] -> [a]
eliminar 0 xs = xs                            -- CASO BASE 1
eliminar _ [] = []                            -- CASO BASE 2
eliminar n (_:xs) = eliminar (n-1) xs        -- CASO RECURSIVO

-- Elemento en posición n
elemento :: Int -> [a] -> Maybe a
elemento _ [] = Nothing                       -- CASO BASE 1
elemento 0 (x:_) = Just x                     -- CASO BASE 2
elemento n (_:xs) = elemento (n-1) xs        -- CASO RECURSIVO

-- ====================================================================
-- 5. RECURSIÓN CON PREDICADOS
-- ====================================================================

-- Filtrar elementos que cumplen una condición
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []                             -- CASO BASE
filtrar p (x:xs)
  | p x = x : filtrar p xs                    -- Include x
  | otherwise = filtrar p xs                  -- Skip x

-- Todos los elementos cumplen la condición
todos :: (a -> Bool) -> [a] -> Bool
todos _ [] = True                             -- CASO BASE
todos p (x:xs) = p x && todos p xs           -- CASO RECURSIVO

-- Algún elemento cumple la condición
alguno :: (a -> Bool) -> [a] -> Bool
alguno _ [] = False                           -- CASO BASE
alguno p (x:xs) = p x || alguno p xs         -- CASO RECURSIVO

-- ====================================================================
-- 6. RECURSIÓN CON ACUMULADORES (Tail Recursion)
-- ====================================================================

-- Factorial con acumulador (más eficiente)
factorialAcc :: Int -> Int
factorialAcc n = factorialAux n 1
  where
    factorialAux 0 acc = acc                 -- CASO BASE
    factorialAux n acc = factorialAux (n-1) (n * acc)  -- CASO RECURSIVO

-- Suma con acumulador
sumaListaAcc :: Num a => [a] -> a
sumaListaAcc xs = sumaAux xs 0
  where
    sumaAux [] acc = acc                     -- CASO BASE
    sumaAux (x:xs) acc = sumaAux xs (x + acc)  -- CASO RECURSIVO

-- Reversa con acumulador
reversaAcc :: [a] -> [a]
reversaAcc xs = reversaAux xs []
  where
    reversaAux [] acc = acc                   -- CASO BASE
    reversaAux (x:xs) acc = reversaAux xs (x:acc)  -- CASO RECURSIVO

-- ====================================================================
-- 7. RECURSIÓN MUTUA
-- ====================================================================

-- Funciones que se llaman mutuamente
esPar :: Int -> Bool
esPar 0 = True
esPar n = esImpar (n-1)

esImpar :: Int -> Bool
esImpar 0 = False
esImpar n = esPar (n-1)

-- ====================================================================
-- 8. RECURSIÓN CON ESTRUCTURAS DE DATOS PERSONALIZADAS
-- ====================================================================

-- Árbol binario
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a)

-- Insertar en árbol binario de búsqueda
insertar :: Ord a => a -> Arbol a -> Arbol a
insertar x Vacio = Nodo x Vacio Vacio         -- CASO BASE
insertar x (Nodo y izq der)
  | x <= y = Nodo y (insertar x izq) der      -- Insertar a la izquierda
  | otherwise = Nodo y izq (insertar x der)   -- Insertar a la derecha

-- Buscar en árbol
buscar :: Ord a => a -> Arbol a -> Bool
buscar _ Vacio = False                        -- CASO BASE
buscar x (Nodo y izq der)
  | x == y = True
  | x < y = buscar x izq
  | otherwise = buscar x der

-- Altura del árbol
altura :: Arbol a -> Int
altura Vacio = 0                              -- CASO BASE
altura (Nodo _ izq der) = 1 + max (altura izq) (altura der)  -- CASO RECURSIVO

-- ====================================================================
-- 9. PATRONES COMUNES DE RECURSIÓN
-- ====================================================================

-- Map: Aplicar función a cada elemento
mapear :: (a -> b) -> [a] -> [b]
mapear _ [] = []                              -- CASO BASE
mapear f (x:xs) = f x : mapear f xs          -- CASO RECURSIVO

-- Fold: Reducir lista a un solo valor
plegar :: (a -> b -> b) -> b -> [a] -> b
plegar _ acc [] = acc                         -- CASO BASE
plegar f acc (x:xs) = f x (plegar f acc xs)  -- CASO RECURSIVO

-- Zip: Combinar dos listas
combinar :: [a] -> [b] -> [(a,b)]
combinar [] _ = []                            -- CASO BASE 1
combinar _ [] = []                            -- CASO BASE 2
combinar (x:xs) (y:ys) = (x,y) : combinar xs ys  -- CASO RECURSIVO

-- ====================================================================
-- 10. CONSEJOS Y MEJORES PRÁCTICAS
-- ====================================================================

{-
CONSEJOS PARA ESCRIBIR FUNCIONES RECURSIVAS:

1. IDENTIFICA EL CASO BASE:
   - ¿Cuál es el caso más simple?
   - ¿Cuándo debe detenerse la recursión?

2. DEFINE EL CASO RECURSIVO:
   - ¿Cómo puedo hacer el problema más pequeño?
   - ¿Cómo combino el resultado actual con el resultado recursivo?

3. ASEGÚRATE DEL PROGRESO:
   - Cada llamada recursiva debe acercarse al caso base
   - Evita recursión infinita

4. USA PATTERN MATCHING:
   - Descompón las estructuras de datos
   - Maneja todos los casos posibles

5. CONSIDERA LA EFICIENCIA:
   - Usa acumuladores para tail recursion
   - Evita cálculos repetidos (memoization)

ERRORES COMUNES:
- Olvidar el caso base
- No hacer progreso hacia el caso base
- Pattern matching incompleto
- Stack overflow por recursión muy profunda
-}

-- ====================================================================
-- 11. EJEMPLOS DE PRÁCTICA
-- ====================================================================

-- Ejercicio 1: Contar elementos que cumplen una condición


-- Ejercicio 2: Aplanar lista de listas


-- Ejercicio 3: Intercalar dos listas


-- Ejercicio 4: Generar lista de números de n a 1


-- Ejercicio 5: Palíndromo

