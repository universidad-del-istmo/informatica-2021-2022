-------------------------------------------------------
--Universidad del ISTMO
-- Informática 1
-- Kimberly Barrera
-------------------------------------------------------
-- actividad: 
-- lab #6
-------------------------------------------------------
-- integrantes: 
-- Angel René Murga Roche 
-- Marvin Rodrigo Rivas Quex 

-- Nota: En los siguientes ejercicios ya no se hace uso de "cons" a la hora de trabajar con listas,
-- [1,2,3] es lo equivalente a (Cons 1 (Cons 2 (Cons 3 Nil)))

-- Fubción Fold
foldl2 :: (a -> b -> a) -> a -> [b] -> a
foldl2 f z [] = z
foldl2 f z (x:xs) = foldl2 f (f z x) xs

-- Problema #1
-- Utilize la funcion "fold" para implementar
-- la funcion "fromDigits". Esta funcion debe
-- aceptar una lista con numeros del 0 al 9 y
-- producir como resultado el numero representado
-- por los digitos de esa lista
--
-- Ejemplo:
-- fromDigits (Cons 1 (Cons 2 (Cons 3 Nil))) == 123


exponente :: Int-> [Int] -> Int
exponente n (x:xs) = x*(10^n) +  exponente (n+1)xs

potencia :: Int->Int->Int
potencia n x = 10*n+x

fromDigits :: [Int] -> Int
fromDigits [] = 0
fromDigits (x:xs)= foldl2 potencia 0 (x:xs) 


-- Problema #2
-- Utilize la funcion fold para implementar
-- la funcion "minMax". Esta funcion acepta
-- una lista de numeros y retorna una pareja
-- ordenada con el minimo y el maximo de esa lista.
--
-- Ejemplo
-- minMax (Cons 5 (Cons 2 (Cons 1 (Cons 10 (Cons 8 Nil))))) == (1, 10)

minMax :: [Int] -> (Int,Int)
minMax (x:xs) = (foldl2 (min) x (x:xs), foldl2 (max) x (x:xs) ) 


-- Problema #3
-- Utrilize la funcion fold para implementar
-- la funcion "minMaxBy". Esta funcion es similar a
-- la funcion anterior pero generalizada para
-- cualquier tipo de valor en la lista. Debe aceptar
-- como un parametro extra el criterio que se utilizara
-- para comparar los valores de la lista
-- 
-- Ejemplo:
-- compararInts a b = a >= b
-- minMaxBy compararInts (Cons 5 (Cons 2 (Cons 1 (Cons 10 (Cons 8 Nil))))) == (1,10)
-- compararIntsInv a b = a <= b
-- minMaxBy compararIntsInv (Cons 5 (Cons 2 (Cons 1 (Cons 10 (Cons 8 Nil))))) == (10, 1)

--minMaxBy :: (a -> a -> Bool) -> [a] -> (a, a)
--minMaxBy :: String -> [Int] -> (Int, Int)
minMaxBy a (x:xs) = case a of 
    "compararInt" -> minMax (x:xs)
    "compararIntInv" -> maxMin (x:xs)

maxMin :: [Int] -> (Int,Int)
maxMin (x:xs)=(foldl2 (max) x (x:xs), foldl2 (min) x (x:xs) ) 