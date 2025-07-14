-- ====================================================================
-- CURRIFICACIÓN EN HASKELL - GUÍA COMPLETA
-- ====================================================================

-- ====================================================================
-- 1. ¿QUÉ ES LA CURRIFICACIÓN?
-- ====================================================================

{-
La currificación es el proceso de transformar una función que toma 
múltiples argumentos en una cadena de funciones que toman un solo 
argumento cada una.

En lugar de: f(x, y, z) = resultado
Tenemos:     f(x)(y)(z) = resultado

En Haskell, TODAS las funciones están currificadas por defecto.
-}

-- ====================================================================
-- 2. EJEMPLOS BÁSICOS
-- ====================================================================

-- Función tradicional de suma (conceptualmente)
-- suma :: (Int, Int) -> Int        -- NO currificada (tupla)
-- suma (x, y) = x + y

-- Función currificada (forma estándar en Haskell)
suma :: Int -> Int -> Int           -- Currificada
suma x y = x + y

-- Lo que realmente significa la signatura:
-- suma :: Int -> (Int -> Int)
-- Es decir: "una función que toma un Int y devuelve una función que toma un Int y devuelve un Int"

ejemploCurrificacion :: IO ()
ejemploCurrificacion = do
    putStrLn "=== Ejemplo de Currificación ==="
    
    -- Aplicación parcial automática
    let suma5 = suma 5              -- suma5 :: Int -> Int
    putStrLn $ "suma 5 3 = " ++ show (suma 5 3)
    putStrLn $ "suma5 3 = " ++ show (suma5 3)
    
    -- Equivalencia de aplicación
    let resultado1 = suma 5 3       -- Aplicación completa
    let resultado2 = (suma 5) 3     -- Aplicación paso a paso
    putStrLn $ "Ambos dan: " ++ show resultado1 ++ " y " ++ show resultado2

-- ====================================================================
-- 3. APLICACIÓN PARCIAL
-- ====================================================================

-- La currificación permite aplicación parcial naturalmente
multiplicar :: Int -> Int -> Int
multiplicar x y = x * y

-- Creamos funciones especializadas
doble :: Int -> Int
doble = multiplicar 2               -- Aplicación parcial

triple :: Int -> Int  
triple = multiplicar 3

cuadrado :: Int -> Int
cuadrado x = multiplicar x x        -- Usando la función original

ejemplosAplicacionParcial :: IO ()
ejemplosAplicacionParcial = do
    putStrLn "=== Aplicación Parcial ==="
    putStrLn $ "doble 7 = " ++ show (doble 7)
    putStrLn $ "triple 4 = " ++ show (triple 4)
    putStrLn $ "cuadrado 5 = " ++ show (cuadrado 5)

-- ====================================================================
-- 4. CURRIFICACIÓN MANUAL CON TUPLAS
-- ====================================================================

-- Función que recibe tupla (NO currificada)
sumaEnTupla :: (Int, Int) -> Int
sumaEnTupla (x, y) = x + y

-- Función currificada
sumaCurrificada :: Int -> Int -> Int
sumaCurrificada x y = x + y

-- Función para currificar manualmente
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

-- Función para des-currificar (uncurry)
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (x, y) = f x y

ejemploCurryUncurry :: IO ()
ejemploCurryUncurry = do
    putStrLn "=== Curry y Uncurry ==="
    
    -- Usar función con tupla
    putStrLn $ "sumaEnTupla (3, 4) = " ++ show (sumaEnTupla (3, 4))
    
    -- Currificar la función de tupla
    let sumaCurr = curry' sumaEnTupla
    putStrLn $ "curry sumaEnTupla 3 4 = " ++ show (sumaCurr 3 4)
    
    -- Des-currificar función currificada
    let sumaUncurr = uncurry' sumaCurrificada
    putStrLn $ "uncurry sumaCurrificada (3, 4) = " ++ show (sumaUncurr (3, 4))

-- ====================================================================
-- 5. EJEMPLOS CON TRES ARGUMENTOS
-- ====================================================================

-- Función con tres argumentos
sumarTres :: Int -> Int -> Int -> Int
sumarTres x y z = x + y + z

-- Interpretación: Int -> (Int -> (Int -> Int))

ejemplosTresArgumentos :: IO ()
ejemplosTresArgumentos = do
    putStrLn "=== Tres Argumentos ==="
    
    -- Diferentes niveles de aplicación parcial
    let paso1 = sumarTres 10            -- paso1 :: Int -> Int -> Int
    let paso2 = paso1 20                -- paso2 :: Int -> Int  
    let resultado = paso2 30            -- resultado :: Int
    
    putStrLn $ "sumarTres 10 20 30 = " ++ show (sumarTres 10 20 30)
    putStrLn $ "Paso a paso: " ++ show resultado
    
    -- Crear funciones especializadas
    let sumar10 = sumarTres 10          -- Suma 10 + y + z
    let sumar10y20 = sumarTres 10 20    -- Suma 10 + 20 + z
    
    putStrLn $ "sumar10 5 7 = " ++ show (sumar10 5 7)
    putStrLn $ "sumar10y20 5 = " ++ show (sumar10y20 5)

-- ====================================================================
-- 6. CURRIFICACIÓN CON FUNCIONES DE ORDEN SUPERIOR
-- ====================================================================

-- Map currificado
mapear :: (a -> b) -> [a] -> [b]
mapear _ [] = []
mapear f (x:xs) = f x : mapear f xs

-- Filter currificado  
filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar p (x:xs)
    | p x = x : filtrar p xs
    | otherwise = filtrar p xs

-- Fold currificado
plegar :: (a -> b -> b) -> b -> [a] -> b
plegar _ acc [] = acc
plegar f acc (x:xs) = f x (plegar f acc xs)

ejemplosFuncionesOrdenSuperior :: IO ()
ejemplosFuncionesOrdenSuperior = do
    putStrLn "=== Funciones de Orden Superior Currificadas ==="
    
    let numeros = [1, 2, 3, 4, 5]
    
    -- Crear funciones especializadas por aplicación parcial
    let doblar = mapear (* 2)           -- doblar :: [Int] -> [Int]
    let soloImpares = filtrar odd       -- soloImpares :: [Int] -> [Int]
    let sumarTodos = plegar (+) 0       -- sumarTodos :: [Int] -> Int
    
    putStrLn $ "Números: " ++ show numeros
    putStrLn $ "Doblar: " ++ show (doblar numeros)
    putStrLn $ "Solo impares: " ++ show (soloImpares numeros)
    putStrLn $ "Suma total: " ++ show (sumarTodos numeros)
    
    -- Composición de funciones currificadas
    let procesarLista = sumarTodos . doblar . soloImpares
    putStrLn $ "Procesar (impares, doblar, sumar): " ++ show (procesarLista numeros)

-- ====================================================================
-- 7. CURRIFICACIÓN EN LA PRÁCTICA
-- ====================================================================

-- Función para crear predicados
mayorQue :: Int -> Int -> Bool
mayorQue limite valor = valor > limite

-- Función para crear transformaciones
multiplicarPor :: Double -> Double -> Double
multiplicarPor factor valor = valor * factor

-- Función para crear validadores
estaEnRango :: Int -> Int -> Int -> Bool
estaEnRango minimo maximo valor = valor >= minimo && valor <= maximo

ejemplosPracticos :: IO ()
ejemplosPracticos = do
    putStrLn "=== Ejemplos Prácticos ==="
    
    let numeros = [1, 5, 10, 15, 20, 25]
    
    -- Crear predicados especializados
    let mayorQue10 = mayorQue 10
    let mayorQue15 = mayorQue 15
    
    putStrLn $ "Números: " ++ show numeros
    putStrLn $ "Mayor que 10: " ++ show (filter mayorQue10 numeros)
    putStrLn $ "Mayor que 15: " ++ show (filter mayorQue15 numeros)
    
    -- Crear transformaciones especializadas
    let porcentaje = multiplicarPor 0.01
    let duplicar = multiplicarPor 2
    
    let precios = [100, 200, 300]
    putStrLn $ "Precios: " ++ show precios
    putStrLn $ "Como porcentaje: " ++ show (map porcentaje (map fromIntegral precios))
    putStrLn $ "Duplicados: " ++ show (map duplicar (map fromIntegral precios))
    
    -- Crear validadores especializados
    let estaEntre1y10 = estaEnRango 1 10
    let estaEntre10y20 = estaEnRango 10 20
    
    putStrLn $ "Entre 1 y 10: " ++ show (filter estaEntre1y10 numeros)
    putStrLn $ "Entre 10 y 20: " ++ show (filter estaEntre10y20 numeros)

-- ====================================================================
-- 8. VENTAJAS DE LA CURRIFICACIÓN
-- ====================================================================

{-
VENTAJAS DE LA CURRIFICACIÓN:

1. REUTILIZACIÓN DE CÓDIGO:
   - Crear funciones especializadas fácilmente
   - Aplicación parcial natural

2. COMPOSICIÓN DE FUNCIONES:
   - Las funciones de un argumento se componen mejor
   - Pipelines de transformación más limpios

3. FUNCIONES DE ORDEN SUPERIOR:
   - Más flexibles y reutilizables
   - Mejor integración con map, filter, fold

4. LEGIBILIDAD:
   - Código más expresivo
   - Menos parámetros explícitos

5. OPTIMIZACIÓN:
   - El compilador puede optimizar mejor
   - Evaluación perezosa más eficiente
-}

-- ====================================================================
-- 9. EJEMPLOS AVANZADOS
-- ====================================================================

-- Currificación con funciones de múltiples tipos
configurarConexion :: String -> Int -> String -> Bool -> String
configurarConexion host puerto usuario ssl = 
    "Conectando a " ++ host ++ ":" ++ show puerto ++ 
    " usuario=" ++ usuario ++ " ssl=" ++ show ssl

-- Crear configuraciones específicas
conexionLocal :: String -> Bool -> String
conexionLocal = configurarConexion "localhost" 3306

conexionProduccion :: String -> Bool -> String  
conexionProduccion = configurarConexion "prod.example.com" 5432

-- Pipeline de transformación usando currificación
procesar :: [Int] -> [Int]
procesar = map (* 2) . filter (> 5) . map (+ 1)

-- Equivalente paso a paso:
-- procesar xs = map (* 2) (filter (> 5) (map (+ 1) xs))

ejemplosAvanzados :: IO ()
ejemplosAvanzados = do
    putStrLn "=== Ejemplos Avanzados ==="
    
    -- Configuraciones de conexión
    putStrLn $ conexionLocal "admin" True
    putStrLn $ conexionProduccion "user" False
    
    -- Pipeline de procesamiento
    let datos = [1, 3, 5, 7, 9, 11]
    putStrLn $ "Datos originales: " ++ show datos
    putStrLn $ "Procesados: " ++ show (procesar datos)

-- ====================================================================
-- 10. EJERCICIOS PRÁCTICOS
-- ====================================================================

{-
EJERCICIOS PARA PRACTICAR CURRIFICACIÓN:

1. Crea una función currificada que tome nombre, apellido y edad,
   y devuelva una presentación formateada.

2. Implementa una función de validación currificada que tome
   límite mínimo, máximo y una lista, y devuelva elementos válidos.

3. Crea un sistema de descuentos currificado donde puedas aplicar
   diferentes tipos de descuento a productos.

4. Implementa una función de logging currificada que tome nivel,
   timestamp y mensaje.

5. Crea un parser simple currificado que tome separador, formato
   y cadena de texto.
-}

-- Ejercicio 1: Presentación
presentar :: String -> String -> Int -> String
presentar nombre apellido edad = 
    "Hola, soy " ++ nombre ++ " " ++ apellido ++ 
    " y tengo " ++ show edad ++ " años"

-- Ejercicio 2: Validación con rango
validarRango :: Int -> Int -> [Int] -> [Int]
validarRango minimo maximo = filter (\x -> x >= minimo && x <= maximo)

-- Ejercicio 3: Sistema de descuentos  
aplicarDescuento :: Double -> Double -> Double
aplicarDescuento porcentaje precio = precio * (1 - porcentaje / 100)

-- Ejercicio 4: Logging
log' :: String -> String -> String -> String
log' nivel timestamp mensaje = 
    "[" ++ timestamp ++ "] " ++ nivel ++ ": " ++ mensaje

-- Ejercicio 5: Parser simple
parsear :: Char -> (String -> a) -> String -> [a]
parsear separador convertir texto = 
    map convertir (words (map (\c -> if c == separador then ' ' else c) texto))

ejemplosEjercicios :: IO ()
ejemplosEjercicios = do
    putStrLn "=== Ejercicios Resueltos ==="
    
    -- Ejercicio 1
    let presentarJuan = presentar "Juan"
    putStrLn $ presentarJuan "Pérez" 25
    
    -- Ejercicio 2
    let validar1a10 = validarRango 1 10
    putStrLn $ "Válidos (1-10): " ++ show (validar1a10 [0, 5, 10, 15])
    
    -- Ejercicio 3
    let descuento20 = aplicarDescuento 20
    putStrLn $ "Precio con 20% desc: " ++ show (descuento20 100)
    
    -- Ejercicio 4
    let logError = log' "ERROR" "2025-07-13"
    putStrLn $ logError "Algo salió mal"
    
    -- Ejercicio 5
    let parsearNumeros = parsear ',' read
    putStrLn $ "Números parseados: " ++ show (parsearNumeros "1,2,3,4,5" :: [Int])

-- ====================================================================
-- FUNCIÓN PRINCIPAL PARA DEMOSTRAR TODO
-- ====================================================================

main :: IO ()
main = do
    putStrLn "======================================================================"
    putStrLn "CURRIFICACIÓN EN HASKELL - DEMOSTRACIÓN COMPLETA"
    putStrLn "======================================================================"
    putStrLn ""
    
    ejemploCurrificacion
    putStrLn ""
    
    ejemplosAplicacionParcial  
    putStrLn ""
    
    ejemploCurryUncurry
    putStrLn ""
    
    ejemplosTresArgumentos
    putStrLn ""
    
    ejemplosFuncionesOrdenSuperior
    putStrLn ""
    
    ejemplosPracticos
    putStrLn ""
    
    ejemplosAvanzados
    putStrLn ""
    
    ejemplosEjercicios
    putStrLn ""
    
    putStrLn "¡Currificación completada! 🎉"
