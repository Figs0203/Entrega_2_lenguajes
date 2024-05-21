-- Función para calcular la suma de los divisores propios de un número
divisoresPropios :: Int -> Int
divisoresPropios n = sum [x | x <- [1..(n `div` 2)], n `mod` x == 0]

-- Función para clasificar un número como perfecto, abundante o deficiente
clasificacionNumerica :: Int -> String
clasificacionNumerica n
    | suma == n = "perfect"
    | suma > n = "abundant"
    | otherwise = "deficient"
    where suma = divisoresPropios n

-- Función para obtener la categoría del programa según la clasificación del número
obtenerCategoriaPrograma :: Int -> String
obtenerCategoriaPrograma n = case clasificacionNumerica n of
    "abundant" -> "Administratives"
    "perfect" -> "Engineering"
    "deficient" -> "Humanities"
    _ -> error "Clasificación inválida"

-- Función para determinar el período de admisión
periodoDeAdmision :: Int -> String
periodoDeAdmision codigo = 
    let año = (codigo `div` 1000000) `mod` 100
        semestre = if (codigo `div` 100000) `mod` 10 == 1 then "1" else "2"
    in show (2000 + año) ++ "-" ++ semestre

-- Función para extraer el número de orden del código del estudiante
obtenerNumeroOrden :: Int -> Int
obtenerNumeroOrden codigo = codigo `mod` 1000

-- Función para verificar si un número es par o impar
esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

-- Función principal para analizar el código del estudiante
analizarCodigoEstudiante :: Int -> String
analizarCodigoEstudiante codigo =
    let periodo = periodoDeAdmision codigo
        categoria = obtenerCategoriaPrograma (codigo `div` 1000 `mod` 100)
        numeroOrden = obtenerNumeroOrden codigo
        paridad = if esPar codigo then "even" else "odd"
    in periodo ++ " " ++ categoria ++ " num" ++ show numeroOrden ++ " " ++ paridad

-- Función principal de entrada
main :: IO ()
main = do
    input <- getLine
    let codigo = read input :: Int
    putStrLn $ analizarCodigoEstudiante codigo
