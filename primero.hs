
-- 1) primer problema
import Text.Printf

-- Definición de las funciones matemáticas
sinFunc :: Double -> Double
sinFunc x = sin x

cosFunc :: Double -> Double
cosFunc x = cos x

tanFunc :: Double -> Double
tanFunc x = tan x

expFunc :: Double -> Double
expFunc x = exp x

logFunc :: Double -> Double
logFunc x = log x

-- Función principal que solicita al usuario el valor y la función a aplicar
calcularFuncion :: IO ()
calcularFuncion = do
    putStrLn "Ingrese el valor para calcular la función:"
    valorStr <- getLine
    let valor = read valorStr :: Double
    putStrLn "Seleccione la función a aplicar:"
    putStrLn "1. Seno"
    putStrLn "2. Coseno"
    putStrLn "3. Tangente"
    putStrLn "4. Exponencial"
    putStrLn "5. Logaritmo Neperiano"
    opcionStr <- getLine
    let opcion = read opcionStr :: Int
    putStrLn "Tabla de resultados:"
    putStrLn "---------------------"
    putStrLn "Valor | Resultado"
    putStrLn "---------------------"
    case opcion of
        1 -> imprimirTabla (sinFunc) valor
        2 -> imprimirTabla (cosFunc) valor
        3 -> imprimirTabla (tanFunc) valor
        4 -> imprimirTabla (expFunc) valor
        5 -> imprimirTabla (logFunc) valor
        _ -> putStrLn "Opción no válida"

-- Función auxiliar para imprimir la tabla de resultados
imprimirTabla :: (Double -> Double) -> Double -> IO ()
imprimirTabla funcion valor = mapM_ (\x -> printf "%-6.2f | %-10.6f\n" x (funcion x)) [1..valor]

-- Función principal para ejecutar el programa
main :: IO ()
main = calcularFuncion

