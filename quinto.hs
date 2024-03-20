type Año = Int
type Metros = Int
type Habitaciones = Int
type Garaje = Bool
type Zona = Char
type Precio = Float

type Inmueble = (Año, Metros, Habitaciones, Garaje, Zona)

-- Función para calcular el precio de un inmueble
calcularPrecio :: Inmueble -> Precio
calcularPrecio (año, metros, habitaciones, garaje, zona)
    | zona == 'A' = fromIntegral (metros * 1000 + habitaciones * 5000 + if garaje then 15000 else 0) * (1 - fromIntegral (2024 - año) / 100)
    | zona == 'B' = fromIntegral (metros * 1000 + habitaciones * 5000 + if garaje then 15000 else 0) * (1 - fromIntegral (2024 - año) / 100) * 1.5
    | otherwise   = 0

-- Función para buscar inmuebles dentro de un presupuesto
buscarInmuebles :: [Inmueble] -> Precio -> [(Inmueble, Precio)]
buscarInmuebles [] _ = []
buscarInmuebles (inmueble:resto) presupuesto =
    let precio = calcularPrecio inmueble
    in if precio <= presupuesto
        then (inmueble, precio) : buscarInmuebles resto presupuesto
        else buscarInmuebles resto presupuesto

-- Ejemplo de uso:
inmuebles :: [Inmueble]
inmuebles = [
    (2000, 100, 3, True, 'A'),
    (2012, 60, 2, True, 'B'),
    (1980, 120, 4, False, 'A'),
    (2005, 75, 3, True, 'B'),
    (2015, 90, 2, False, 'A')
  ]

presupuesto :: Precio
presupuesto = 100000

-- Resultado de búsqueda de inmuebles dentro del presupuesto
inmueblesEncontrados :: [(Inmueble, Precio)]
inmueblesEncontrados = buscarInmuebles inmuebles presupuesto

-- Función para imprimir un inmueble con su precio
mostrarInmueble :: (Inmueble, Precio) -> String
mostrarInmueble ((año, metros, habitaciones, garaje, zona), precio) =
    "Año: " ++ show año ++ ", Metros: " ++ show metros ++ ", Habitaciones: " ++ show habitaciones ++ ", Garaje: " ++ show garaje ++ ", Zona: " ++ [zona] ++ ", Precio: " ++ show precio


main :: IO ()
main = mapM_ (putStrLn . mostrarInmueble) inmueblesEncontrados