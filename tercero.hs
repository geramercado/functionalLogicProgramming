calificaciones :: [Int] -> [String]
calificaciones [] = []
calificaciones (x:xs)
    | x >= 90 && x <= 100 = "A" : calificaciones xs
    | x >= 80 && x < 90   = "B" : calificaciones xs
    | x >= 70 && x < 80   = "C" : calificaciones xs
    | x >= 60 && x < 70   = "D" : calificaciones xs
    | otherwise            = "F" : calificaciones xs

-- Ejemplo de uso:
notas :: [Int]
notas = [95, 85, 75, 65, 55]

calif :: [String]
calif = calificaciones notas

-- Imprimimos el resultado
main :: IO ()
main = print calif