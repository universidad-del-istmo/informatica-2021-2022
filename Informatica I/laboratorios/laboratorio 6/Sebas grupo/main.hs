module Main where


data Lista a = Cons a (Lista a) | Nil deriving Show

fold fCons fNil Nil = fNil
fold fCons fNil (Cons x xs) = fCons x (fold fCons fNil xs)

mapAcc f x = Cons (f x)

map f = fold (mapAcc f) Nil

-- Problema #1
-- Utilize la funcion "fold" para implementar
-- la funcion "fromDigits". Esta funcion debe
-- aceptar una lista con numeros del 0 al 9 y
-- producir como resultado el numero representado
-- por los digitos de esa lista
--
-- Ejemplo:
-- fromDigits (Cons 1 (Cons 2 (Cons 3 Nil))) == 123


-- Solucion y descripcion
fromDigits :: Lista Int -> Int
fromDigits [] = 0
fromDigits (x:xs) = foldr (Cons ++ [0 .. 9])

fromDigits [] = 0
fromDigits (x:xs) =  x*(10^(length(x:xs)-1)) + fromDigits xs
-- Lo que quise hacer en esta funcion de arriba es que el fold al implementar la misma funcion a toda la lista
-- lo que va a hacer es que toma el Cons de 0 hasta el cons de 9 y lo que va a hacer es que con el ++ es 
-- que los va a unir y al final va a dar el numero 0123456789. Esa era mi idea pero no encontre la manera
-- de implementarlo bien. 
-- la segunda parte de la funcion esta resuelta sin fold. 



-- Problema #2
-- Utilize la funcion fold para implementar
-- la funcion "minMax". Esta funcion acepta
-- una lista de numeros y retorna una pareja
-- ordenada con el minimo y el maximo de esa lista.
--
-- Ejemplo
-- minMax (Cons 5 (Cons 2 (Cons 1 (Cons 10 (Cons 8 Nil))))) == (1, 10)


-- Solucion y descripcion 
min = a < b
max = a > b
--max :: forall a. Ord a => a -> a -> a
-- minMax :: forall a. (Ord a, Ord a) => a -> a -> a
minMax :: Lista Int -> (Int, Int)
minMax (x:xs) = (foldr (minimum (t a),maximum (t a)) -> [x .. xs])

minMax xs = (minimum xs, maximum xs)
-- En esta funcion lo que queria hacer era con el foldr aplicar el minimo y el maxiomo de la lista definida
-- que es el rango de x que sera head y el xs que es el tail. Y ya definido el rango lo que va a hacer la 
-- funcion es encontrar el minimo y el maximo. 
-- la segunda funcion esta resulta solo que esta resuelta sin fold.


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


-- Solucion y descripcion 
minMaxBy :: (a -> a -> Bool) -> Lista a -> (a, a)
minMaxBy = (foldl ())

if a b = b a
    then compararInts == ComprararIntsInv
    else 0

if (1,10) = (10,1)
    then comprararInts == compararIntsInv
    else 0

minMaxBy xs "compararInts" =  (minimum xs, maximum xs)
minMaxBy xs "compararIntsInv" =  (maximum xs, minimum xs)

-- En esta funcion intente implementar el in then y else pero si me perdi mucho en la implementacion
-- de fold, entonces mejor lo decidi hacer sin fold, entonces la funcion esta resuleta solo que sin el uso 
-- de fold

main = undefined
