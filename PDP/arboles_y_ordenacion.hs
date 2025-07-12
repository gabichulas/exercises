-- ====================================================================
-- MASTERCLASS: ÁRBOLES Y ALGORITMOS DE ORDENACIÓN EN HASKELL
-- ====================================================================

-- ====================================================================
-- 1. ÁRBOLES BINARIOS - DEFINICIÓN Y OPERACIONES BÁSICAS
-- ====================================================================

-- Definición de un árbol binario
data Arbol a = Vacio | Nodo a (Arbol a) (Arbol a)
  deriving (Show, Eq)

-- Ejemplos de árboles
ejemploArbol1 :: Arbol Int
ejemploArbol1 = Nodo 5 
                  (Nodo 3 (Nodo 1 Vacio Vacio) (Nodo 4 Vacio Vacio))
                  (Nodo 7 (Nodo 6 Vacio Vacio) (Nodo 9 Vacio Vacio))

ejemploArbol2 :: Arbol Char
ejemploArbol2 = Nodo 'D'
                  (Nodo 'B' (Nodo 'A' Vacio Vacio) (Nodo 'C' Vacio Vacio))
                  (Nodo 'F' (Nodo 'E' Vacio Vacio) (Nodo 'G' Vacio Vacio))

-- ====================================================================
-- 2. OPERACIONES BÁSICAS CON ÁRBOLES
-- ====================================================================

-- Altura del árbol
altura :: Arbol a -> Int
altura Vacio = 0
altura (Nodo _ izq der) = 1 + max (altura izq) (altura der)

-- Contar nodos
contarNodos :: Arbol a -> Int
contarNodos Vacio = 0
contarNodos (Nodo _ izq der) = 1 + contarNodos izq + contarNodos der

-- Contar hojas (nodos sin hijos)
contarHojas :: Arbol a -> Int
contarHojas Vacio = 0
contarHojas (Nodo _ Vacio Vacio) = 1  -- Es una hoja
contarHojas (Nodo _ izq der) = contarHojas izq + contarHojas der

-- Buscar un elemento en el árbol
buscarElemento :: Eq a => a -> Arbol a -> Bool
buscarElemento _ Vacio = False
buscarElemento x (Nodo y izq der)
  | x == y = True
  | otherwise = buscarElemento x izq || buscarElemento x der

-- Profundidad de un elemento
profundidad :: Eq a => a -> Arbol a -> Maybe Int
profundidad _ Vacio = Nothing
profundidad x (Nodo y izq der)
  | x == y = Just 0
  | otherwise = case (profundidad x izq, profundidad x der) of
      (Just p, _) -> Just (p + 1)
      (_, Just p) -> Just (p + 1)
      (Nothing, Nothing) -> Nothing

-- ====================================================================
-- 3. RECORRIDOS DE ÁRBOLES (TRAVERSALS)
-- ====================================================================

-- Recorrido en orden (In-order): Izquierda -> Raíz -> Derecha
enOrden :: Arbol a -> [a]
enOrden Vacio = []
enOrden (Nodo x izq der) = enOrden izq ++ [x] ++ enOrden der

-- Recorrido pre-orden (Pre-order): Raíz -> Izquierda -> Derecha
preOrden :: Arbol a -> [a]
preOrden Vacio = []
preOrden (Nodo x izq der) = [x] ++ preOrden izq ++ preOrden der

-- Recorrido post-orden (Post-order): Izquierda -> Derecha -> Raíz
postOrden :: Arbol a -> [a]
postOrden Vacio = []
postOrden (Nodo x izq der) = postOrden izq ++ postOrden der ++ [x]

-- Recorrido por niveles (Level-order/Breadth-first)
porNiveles :: Arbol a -> [a]
porNiveles arbol = porNivelesAux [arbol]
  where
    porNivelesAux [] = []
    porNivelesAux (Vacio:resto) = porNivelesAux resto
    porNivelesAux (Nodo x izq der:resto) = x : porNivelesAux (resto ++ [izq, der])

-- ====================================================================
-- 4. ÁRBOLES BINARIOS DE BÚSQUEDA (BST)
-- ====================================================================

-- Insertar en BST manteniendo la propiedad de orden
insertarBST :: Ord a => a -> Arbol a -> Arbol a
insertarBST x Vacio = Nodo x Vacio Vacio
insertarBST x (Nodo y izq der)
  | x <= y = Nodo y (insertarBST x izq) der
  | otherwise = Nodo y izq (insertarBST x der)

-- Buscar en BST (más eficiente que búsqueda general)
buscarBST :: Ord a => a -> Arbol a -> Bool
buscarBST _ Vacio = False
buscarBST x (Nodo y izq der)
  | x == y = True
  | x < y = buscarBST x izq
  | otherwise = buscarBST x der

-- Encontrar el mínimo en BST
minimoBST :: Ord a => Arbol a -> Maybe a
minimoBST Vacio = Nothing
minimoBST (Nodo x Vacio _) = Just x
minimoBST (Nodo _ izq _) = minimoBST izq

-- Encontrar el máximo en BST
maximoBST :: Ord a => Arbol a -> Maybe a
maximoBST Vacio = Nothing
maximoBST (Nodo x _ Vacio) = Just x
maximoBST (Nodo _ _ der) = maximoBST der

-- Eliminar un nodo de BST
eliminarBST :: Ord a => a -> Arbol a -> Arbol a
eliminarBST _ Vacio = Vacio
eliminarBST x (Nodo y izq der)
  | x < y = Nodo y (eliminarBST x izq) der
  | x > y = Nodo y izq (eliminarBST x der)
  | otherwise = case (izq, der) of
      (Vacio, Vacio) -> Vacio
      (Vacio, _) -> der
      (_, Vacio) -> izq
      (_, _) -> let Just sucesor = minimoBST der
                in Nodo sucesor izq (eliminarBST sucesor der)

-- Crear BST desde lista
desdeLista :: Ord a => [a] -> Arbol a
desdeLista = foldr insertarBST Vacio

-- Validar si un árbol es BST
esBST :: Ord a => Arbol a -> Bool
esBST arbol = esBSTAux arbol Nothing Nothing
  where
    esBSTAux Vacio _ _ = True
    esBSTAux (Nodo x izq der) minVal maxVal =
      dentroRango x minVal maxVal &&
      esBSTAux izq minVal (Just x) &&
      esBSTAux der (Just x) maxVal
    
    dentroRango x Nothing Nothing = True
    dentroRango x (Just minV) Nothing = x > minV
    dentroRango x Nothing (Just maxV) = x < maxV
    dentroRango x (Just minV) (Just maxV) = x > minV && x < maxV

-- ====================================================================
-- 5. ALGORITMOS DE ORDENACIÓN RECURSIVOS
-- ====================================================================

-- ====================================================================
-- 5.1 QUICKSORT
-- ====================================================================

-- Quicksort clásico
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot:resto) = 
  quicksort menores ++ [pivot] ++ quicksort mayores
  where
    menores = [x | x <- resto, x <= pivot]
    mayores = [x | x <- resto, x > pivot]

-- Quicksort con función de comparación personalizada
quicksortPor :: (a -> a -> Ordering) -> [a] -> [a]
quicksortPor _ [] = []
quicksortPor comp (pivot:resto) = 
  quicksortPor comp menores ++ [pivot] ++ quicksortPor comp mayores
  where
    menores = [x | x <- resto, comp x pivot /= GT]
    mayores = [x | x <- resto, comp x pivot == GT]

-- ====================================================================
-- 5.2 MERGESORT
-- ====================================================================

-- Mergesort - Divide y vencerás
mergesort :: Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = merge (mergesort primera) (mergesort segunda)
  where
    (primera, segunda) = dividir xs

-- Dividir lista por la mitad
dividir :: [a] -> ([a], [a])
dividir xs = splitAt (length xs `div` 2) xs

-- Mezclar dos listas ordenadas
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- ====================================================================
-- 5.3 HEAPSORT (usando árboles heap)
-- ====================================================================

-- Tipo de dato para heap (max-heap)
data Heap a = HeapVacio | HeapNodo a (Heap a) (Heap a)
  deriving (Show, Eq)

-- Insertar en heap
insertarHeap :: Ord a => a -> Heap a -> Heap a
insertarHeap x HeapVacio = HeapNodo x HeapVacio HeapVacio
insertarHeap x heap = insertarYBurbujear x heap

-- Función auxiliar para insertar y mantener propiedad heap
insertarYBurbujear :: Ord a => a -> Heap a -> Heap a
insertarYBurbujear x HeapVacio = HeapNodo x HeapVacio HeapVacio
insertarYBurbujear x (HeapNodo y izq der)
  | x <= y = HeapNodo y (insertarYBurbujear x izq) der
  | otherwise = HeapNodo x (insertarYBurbujear y izq) der

-- Extraer máximo del heap
extraerMax :: Ord a => Heap a -> Maybe (a, Heap a)
extraerMax HeapVacio = Nothing
extraerMax (HeapNodo x HeapVacio HeapVacio) = Just (x, HeapVacio)
extraerMax (HeapNodo x izq der) = Just (x, reorganizarHeap izq der)

-- Reorganizar heap después de extraer máximo
reorganizarHeap :: Ord a => Heap a -> Heap a -> Heap a
reorganizarHeap HeapVacio HeapVacio = HeapVacio
reorganizarHeap izq HeapVacio = izq
reorganizarHeap HeapVacio der = der
reorganizarHeap (HeapNodo x1 izq1 der1) (HeapNodo x2 izq2 der2)
  | x1 >= x2 = HeapNodo x1 (reorganizarHeap izq1 der1) (HeapNodo x2 izq2 der2)
  | otherwise = HeapNodo x2 (HeapNodo x1 izq1 der1) (reorganizarHeap izq2 der2)

-- Heapsort usando heap
heapsort :: Ord a => [a] -> [a]
heapsort xs = heapsortAux (foldr insertarHeap HeapVacio xs)
  where
    heapsortAux HeapVacio = []
    heapsortAux heap = case extraerMax heap of
      Nothing -> []
      Just (x, restoHeap) -> x : heapsortAux restoHeap

-- ====================================================================
-- 5.4 TREE SORT (usando BST)
-- ====================================================================

-- Tree sort - insertar en BST y luego recorrer en orden
treesort :: Ord a => [a] -> [a]
treesort xs = enOrden (desdeLista xs)

-- ====================================================================
-- 6. ALGORITMOS DE ORDENACIÓN AVANZADOS
-- ====================================================================

-- ====================================================================
-- 6.1 INSERTION SORT RECURSIVO
-- ====================================================================

insertionsort :: Ord a => [a] -> [a]
insertionsort [] = []
insertionsort (x:xs) = insertar x (insertionsort xs)
  where
    insertar y [] = [y]
    insertar y (z:zs)
      | y <= z = y : z : zs
      | otherwise = z : insertar y zs

-- ====================================================================
-- 6.2 SELECTION SORT RECURSIVO
-- ====================================================================

selectionsort :: Ord a => [a] -> [a]
selectionsort [] = []
selectionsort xs = minimo : selectionsort resto
  where
    minimo = minimum xs
    resto = eliminarPrimero minimo xs
    
    eliminarPrimero _ [] = []
    eliminarPrimero y (z:zs)
      | y == z = zs
      | otherwise = z : eliminarPrimero y zs

-- ====================================================================
-- 7. ANÁLISIS DE COMPLEJIDAD Y COMPARACIÓN
-- ====================================================================

{-
COMPLEJIDAD TEMPORAL DE ALGORITMOS DE ORDENACIÓN:

1. QUICKSORT:
   - Mejor caso: O(n log n)
   - Caso promedio: O(n log n)
   - Peor caso: O(n²)
   - Espacio: O(log n)

2. MERGESORT:
   - Todos los casos: O(n log n)
   - Espacio: O(n)
   - Estable (mantiene orden relativo)

3. HEAPSORT:
   - Todos los casos: O(n log n)
   - Espacio: O(1)
   - No estable

4. TREE SORT:
   - Mejor caso: O(n log n)
   - Peor caso: O(n²) - árbol degenerado
   - Espacio: O(n)

5. INSERTION SORT:
   - Mejor caso: O(n)
   - Peor caso: O(n²)
   - Espacio: O(1)
   - Estable

6. SELECTION SORT:
   - Todos los casos: O(n²)
   - Espacio: O(1)
   - No estable
-}

-- ====================================================================
-- 8. ÁRBOLES ESPECIALES
-- ====================================================================

-- ====================================================================
-- 8.1 ÁRBOL AVL (Auto-balanceado)
-- ====================================================================

data ArbolAVL a = AVLVacio | AVLNodo a Int (ArbolAVL a) (ArbolAVL a)
  deriving (Show, Eq)

-- Calcular altura AVL
alturaAVL :: ArbolAVL a -> Int
alturaAVL AVLVacio = 0
alturaAVL (AVLNodo _ h _ _) = h

-- Factor de balance
factorBalance :: ArbolAVL a -> Int
factorBalance AVLVacio = 0
factorBalance (AVLNodo _ _ izq der) = alturaAVL izq - alturaAVL der

-- Crear nodo AVL con altura actualizada
crearNodoAVL :: a -> ArbolAVL a -> ArbolAVL a -> ArbolAVL a
crearNodoAVL x izq der = AVLNodo x nuevaAltura izq der
  where
    nuevaAltura = 1 + max (alturaAVL izq) (alturaAVL der)

-- Rotación a la derecha
rotarDerecha :: ArbolAVL a -> ArbolAVL a
rotarDerecha (AVLNodo y _ (AVLNodo x _ a b) c) = 
  crearNodoAVL x a (crearNodoAVL y b c)
rotarDerecha arbol = arbol

-- Rotación a la izquierda
rotarIzquierda :: ArbolAVL a -> ArbolAVL a
rotarIzquierda (AVLNodo x _ a (AVLNodo y _ b c)) = 
  crearNodoAVL y (crearNodoAVL x a b) c
rotarIzquierda arbol = arbol

-- Insertar en AVL con balanceo
insertarAVL :: Ord a => a -> ArbolAVL a -> ArbolAVL a
insertarAVL x AVLVacio = AVLNodo x 1 AVLVacio AVLVacio
insertarAVL x (AVLNodo y h izq der)
  | x <= y = balancear $ crearNodoAVL y (insertarAVL x izq) der
  | otherwise = balancear $ crearNodoAVL y izq (insertarAVL x der)

-- Balancear árbol AVL
balancear :: ArbolAVL a -> ArbolAVL a
balancear arbol@(AVLNodo x _ izq der)
  | factorBalance arbol > 1 = 
      if factorBalance izq >= 0 
      then rotarDerecha arbol
      else rotarDerecha (AVLNodo x (alturaAVL arbol) (rotarIzquierda izq) der)
  | factorBalance arbol < -1 = 
      if factorBalance der <= 0 
      then rotarIzquierda arbol
      else rotarIzquierda (AVLNodo x (alturaAVL arbol) izq (rotarDerecha der))
  | otherwise = arbol
balancear arbol = arbol

-- ====================================================================
-- 8.2 ÁRBOL ROJO-NEGRO (Simplificado)
-- ====================================================================

data Color = Rojo | Negro deriving (Show, Eq)
data ArbolRN a = RNVacio | RNNodo Color a (ArbolRN a) (ArbolRN a)
  deriving (Show, Eq)

-- Insertar en árbol rojo-negro
insertarRN :: Ord a => a -> ArbolRN a -> ArbolRN a
insertarRN x arbol = hacerNegro (insertarRNAux x arbol)
  where
    hacerNegro (RNNodo _ y izq der) = RNNodo Negro y izq der
    hacerNegro RNVacio = RNVacio

insertarRNAux :: Ord a => a -> ArbolRN a -> ArbolRN a
insertarRNAux x RNVacio = RNNodo Rojo x RNVacio RNVacio
insertarRNAux x (RNNodo color y izq der)
  | x <= y = balancearRN color y (insertarRNAux x izq) der
  | otherwise = balancearRN color y izq (insertarRNAux x der)

-- Balancear árbol rojo-negro
balancearRN :: Color -> a -> ArbolRN a -> ArbolRN a -> ArbolRN a
balancearRN Negro z (RNNodo Rojo y (RNNodo Rojo x a b) c) d = 
  RNNodo Rojo y (RNNodo Negro x a b) (RNNodo Negro z c d)
balancearRN Negro z (RNNodo Rojo x a (RNNodo Rojo y b c)) d = 
  RNNodo Rojo y (RNNodo Negro x a b) (RNNodo Negro z c d)
balancearRN Negro x a (RNNodo Rojo z (RNNodo Rojo y b c) d) = 
  RNNodo Rojo y (RNNodo Negro x a b) (RNNodo Negro z c d)
balancearRN Negro x a (RNNodo Rojo y b (RNNodo Rojo z c d)) = 
  RNNodo Rojo y (RNNodo Negro x a b) (RNNodo Negro z c d)
balancearRN color x izq der = RNNodo color x izq der

-- ====================================================================
-- 9. FUNCIONES DE UTILIDAD Y TESTING
-- ====================================================================

-- Verificar si una lista está ordenada
estaOrdenada :: Ord a => [a] -> Bool
estaOrdenada [] = True
estaOrdenada [_] = True
estaOrdenada (x:y:resto) = x <= y && estaOrdenada (y:resto)

-- Generar lista aleatoria para testing (usando números pseudoaleatorios)
listaEjemplo :: [Int]
listaEjemplo = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42]

-- Benchmark simple (contar comparaciones)
-- Nota: En un sistema real usarías criterion o similar
testOrdenacion :: (Ord a, Show a) => String -> ([a] -> [a]) -> [a] -> IO ()
testOrdenacion nombre algoritmo lista = do
  putStrLn $ "=== " ++ nombre ++ " ==="
  putStrLn $ "Original: " ++ show lista
  let resultado = algoritmo lista
  putStrLn $ "Ordenado: " ++ show resultado
  putStrLn $ "Correcto: " ++ show (estaOrdenada resultado)
  putStrLn ""

-- Función para probar todos los algoritmos
probarTodos :: IO ()
probarTodos = do
  let lista = listaEjemplo
  testOrdenacion "Quicksort" quicksort lista
  testOrdenacion "Mergesort" mergesort lista
  testOrdenacion "Heapsort" heapsort lista
  testOrdenacion "Tree Sort" treesort lista
  testOrdenacion "Insertion Sort" insertionsort lista
  testOrdenacion "Selection Sort" selectionsort lista

-- ====================================================================
-- 10. EJERCICIOS PROPUESTOS
-- ====================================================================

{-
EJERCICIOS PARA PRACTICAR:

1. Implementa una función que verifique si un árbol binario es simétrico
2. Crea una función que convierta un árbol binario a su imagen espejo
3. Implementa búsqueda en profundidad (DFS) y amplitud (BFS) en árboles
4. Crea una función que encuentre el ancestro común más cercano de dos nodos
5. Implementa una función que serialice y deserialice un árbol binario
6. Crea una variante de quicksort que use el elemento mediano como pivot
7. Implementa radix sort para números enteros
8. Crea una función que encuentre el k-ésimo elemento más pequeño en un BST
9. Implementa una función que balance un BST desbalanceado
10. Crea un algoritmo de ordenación estable personalizado

PROYECTOS AVANZADOS:

1. Implementa un árbol B para bases de datos
2. Crea un árbol de segmentos para consultas de rango
3. Implementa un árbol de sufijos para búsqueda de patrones
4. Crea un algoritmo de ordenación externa para archivos grandes
5. Implementa un árbol de decisión para clasificación
-}

-- ====================================================================
-- EJEMPLOS DE USO
-- ====================================================================

ejemplosBST :: IO ()
ejemplosBST = do
  putStrLn "=== Ejemplos de BST ==="
  let arbol = desdeLista [5, 3, 7, 2, 4, 6, 8]
  putStrLn $ "Árbol desde lista [5,3,7,2,4,6,8]:"
  putStrLn $ "En orden: " ++ show (enOrden arbol)
  putStrLn $ "Pre orden: " ++ show (preOrden arbol)
  putStrLn $ "Post orden: " ++ show (postOrden arbol)
  putStrLn $ "Por niveles: " ++ show (porNiveles arbol)
  putStrLn $ "Altura: " ++ show (altura arbol)
  putStrLn $ "Nodos: " ++ show (contarNodos arbol)
  putStrLn $ "Hojas: " ++ show (contarHojas arbol)
  putStrLn $ "Buscar 4: " ++ show (buscarBST 4 arbol)
  putStrLn $ "Buscar 9: " ++ show (buscarBST 9 arbol)
  putStrLn ""
