import Data.Char (toUpper)

type Asignatura = String
type Nota = Int
type AsignaturaNota = (Asignatura, Nota)

-- Función para convertir una asignatura a mayúsculas
toUpperAsignatura :: Asignatura -> Asignatura
toUpperAsignatura = map toUpper

-- Función para obtener la calificación correspondiente a una nota
obtenerCalificacion :: Nota -> String
obtenerCalificacion nota
    | nota >= 95 && nota <= 100 = "Excelente"
    | nota >= 85 && nota < 95   = "Notable"
    | nota >= 75 && nota < 85   = "Bueno"
    | nota >= 70 && nota < 75   = "Suficiente"
    | otherwise                 = "Desempeño insuficiente"

-- Función para procesar el diccionario de asignaturas y notas
procesarCalificaciones :: [(Asignatura, Nota)] -> [(Asignatura, String)]
procesarCalificaciones [] = []
procesarCalificaciones ((asignatura, nota):resto) =
    (toUpperAsignatura asignatura, obtenerCalificacion nota) : procesarCalificaciones resto

-- Ejemplo de uso:
calificacionesAlumno :: [(Asignatura, Nota)]
calificacionesAlumno = [("Matemáticas", 95), ("Física", 80), ("Química", 65), ("Historia", 75)]

calificacionesAprobadas :: [(Asignatura, String)]
calificacionesAprobadas = procesarCalificaciones calificacionesAlumno

-- Imprimimos el resultado
main :: IO ()
main = print calificacionesAprobadas