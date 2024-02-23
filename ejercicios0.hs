module Haskell0 where



-- suma de una lista
sustituirSumando :: [Integer] -> [Integer]
sustituirSumando [] = []
sustituirSumando [x] = [x]
sustituirSumando (x:y:xs) = sustituirSumando (x + y:xs)


-- factorial
factorial :: Integer -> Integer
factorial n = product [1..n]
 
-- par o impar


-- reverso de una lista
reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- enocntrar numeros pares en una lista
numerosPares :: Int -> [Int]
numerosPares n = [x | x <- [0..n], even x]

-- longitud de una cadena
longitudCadena :: String -> Int
longitudCadena cadena = length cadena

-- duplicar elementos de una lista
duplicarElementos :: [Int] -> [Int]
duplicarElementos lista = concatMap (\x -> [x, x]) lista

-- filtrar elementos pares
filtrarPares :: [Int] -> [Int]
filtrarPares lista = filter even lista

-- filtra el n-ésimo numero de la lista fibonacci: es decir el número que está en la posición nindicada
fibonacci :: Int -> Int
fibonacci n
    | n <= 0    = 0
    | n == 1    = 1
    | otherwise = fibonacci (n - 1) + fibonacci (n - 2)

-- divisores
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

-- palindromo
esPalindromo :: String -> Bool
esPalindromo cadena = cadena == reverse cadena


