module Main where 
--Integrantes:
-- Victor Javier Celada Argueta
-- Diego Roberto Garcia Godinez

fold :: (a -> b -> a) -> a -> [b] -> a
fold f ys xs = aux ys xs
    where   aux ys [] = ys
            aux ys (x:xs) = aux (f ys x) xs 

-- Problema #1
-- Utilize la funcion "fold" para implementar
-- la funcion "fromDigits". Esta funcion debe
-- aceptar una lista con numeros del 0 al 9 y
-- producir como resultado el numero representado
-- por los digitos de esa lista
--
-- Ejemplo:
-- fromDigits (Cons 1 (Cons 2 (Cons 3 Nil))) == 123

fromDigits :: [Int] -> Int 
fromDigits [ ] = 0
--fromDigits (x:xs) = fromDigits' (reverse(x:xs)) 1
fromDigits (x:xs) = fold pot 0 (x:xs)
    where pot n x = 10*n+x


--fromDigits' :: [Int] -> Int -> Int
--fromDigits' [ ] n = 0
--fromDigits' (x:xs) n = x * 10^n + fromDigits' (xs) (n+1)


-- Problema #2
-- Utilize la funcion fold para implementar
-- la funcion "minMax". Esta funcion acepta
-- una lista de numeros y retorna una pareja
-- ordenada con el minimo y el maximo de esa lista.
--
-- Ejemplo
-- minMax (Cons 5 (Cons 2 (Cons 1 (Cons 10 (Cons 8 Nil))))) == (1, 10)

minMax :: [Int] -> (Int,Int)
--minMax [ ] = 0
minMax (x:y:xs) = (minimo (x:y:xs), maximo(x:y:xs))

minimo :: [Int] -> Int 
minimo [x] = x 
--minimo (x:y:ys) = minimo ((min x y):ys) 
minimo (x:y:xs) = fold min x (x:y:xs)

maximo :: [Int] -> Int 
maximo [x] = x 
--maximo (x:y:ys) = maximo ((max x y):ys) 
maximo (x:y:xs) = fold max x (x:y:xs)

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

--minMaxby "compararIntInv" [1,2,3,4]
--minMaxby "compararInt" [1,2,3,4]
minMaxby b (x:y:xs) | b == "compararInt" = (minimo (x:y:xs), maximo (x:y:xs))
                    | b == "compararIntInv" = (maximo (x:y:xs), minimo (x:y:xs))

main = undefined 