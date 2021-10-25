----------------------------------------
-- Laboratorio 6
---------------------------------------
-- UNIVERSIDAD DEL ISTMO
-- 1298 - Informática I
-- Ver.: 1.1
-- Fecha: 2021/10/24
--  Diego Giron Figueroa 
-- Jorge Armando Ferguson
-- Laboratorio 6
---------------------------------------
-- Este laboratorio fue asignado por el ingeniero
-- Ernesto por lo cual la estructura a mantener es
-- diferente a la nuva vista en clase
----------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (Int, String, Show, Bool(True, False), undefined, div, mod, show, read, fst, snd, otherwise, (+), (-), (*), (==), (<), (>), (++), (<=), (>=))





data Lista = Nil | Cons Int Lista deriving Show

fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x

mapAcc f x = Cons (f x)

--map f xs = fold (mapAcc f) Nil xs

-- Problema #1
-- Utilize la funcion "fold" para implementar
-- la funcion "fromDigits". Esta funcion debe
-- aceptar una lista con numeros del 0 al 9 y
-- producir como resultado el numero representado
-- por los digitos de esa lista
--
-- Ejemplo:
-- fromDigits (Cons 1 (Cons 2 (Cons 3 Nil))) == 123
auxdigits (a, b) x = ((x * b ) + a, b * 10)

fromDigits :: Lista -> Int
fromDigits lis =  fst ( fold auxdigits (0, 1) lis )


-- Problema #2
-- Utilize la funcion fold para implementar
-- la funcion "minMax". Esta funcion acepta
-- una lista de numeros y retorna una pareja
-- ordenada con el minimo y el maximo de esa lista.
--
-- Ejemplo
-- minMax (Cons 5 (Cons 2 (Cons 1 (Cons 10 (Cons 8 Nil))))) == (1, 10)

--minMax :: Lista Int -> (Int, Int)
--minMax x a b = undefined


minMax' a x
  | x > (snd (a)) = (fst (a), x)
  | x < (fst (a)) = (x, snd (a))
  | otherwise = a

minMax (Cons x xs) = fold minMax' (x, x) xs




-- Problema #3
-- Utrilize la funcion fold para implementar
-- la funcion "minMaxBy". Esta funcion es similar a
-- la funcion anterior pero generalizada para
-- cualquier tipo de valor en la lista. Debe aceptar
-- como un parametro extra el criterio que se utilizara
-- para comparar los valores de la lista
-- 
-- Ejemplo:
compararInts a b = a >= b
-- minMaxBy compararInts (Cons 5 (Cons 2 (Cons 1 (Cons 10 (Cons 8 Nil))))) == (1,10)
compararIntsInv a b = a <= b
-- minMaxBy compararIntsInv (Cons 5 (Cons 2 (Cons 1 (Cons 10 (Cons 8 Nil))))) == (10, 1)

--minMaxBy :: (a -> a -> Bool) -> Lista a -> (a, a)

minMaxAux funcion (x, y) c =
    (if x >= c
        then c
        else x,
        if funcion y c
            then y
            else c)



minMaxBy f (Cons x xs) = fold (minMaxAux f) (x, x) xs



