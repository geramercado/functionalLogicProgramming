filtrar :: (a -> Bool) -> [a] -> [a]
filtrar _ [] = []
filtrar f (x:xs)
    | f x       = x : filtrar f xs
    | otherwise = filtrar f xs

-- Ejemplo de uso:
-- Definimos una función booleana para filtrar los números pares
esPar :: Int -> Bool
esPar x = x `mod` 2 == 0

-- Llamamos a la función filtrar con nuestra función booleana y una lista de ejemplo
resultado :: [Int]
resultado = filtrar esPar [1,2,3,4,5,6,7,8,9,10]

-- Imprimimos el resultado
main :: IO ()
main = print resultado