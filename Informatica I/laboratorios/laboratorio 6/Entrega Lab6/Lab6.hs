--Universidad del Istmo 
--Laboratorio No.6 
--Grupo Formado por:
--Herman Andres echeverria Rojas
--Randy Rodrigo Rivera Mayen


{-# LANGUAGE NoImplicitPrelude #-}
module Lab6 where
import Prelude (Bool(..), (*), fst, snd, (+), (<), (>=), (<=), (>), Int, Show, undefined)
data Lista a = Cons a (Lista a) | Nil deriving Show

foldL fCons fNil Nil = fNil
foldL fCons fNil (Cons x xs) = fCons x (foldL fCons fNil xs)

fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x
pushAgregador estado x = Cons x estado

-- Problema #1
-- Utilize la funcion "fold" para implementar
-- la funcion "fromDigits". Esta funcion debe
-- aceptar una lista con numeros del 0 al 9 y
-- producir como resultado el numero representado
-- por los digitos de esa lista
--
-- Ejemplo:
-- fromDigits (Cons 1 (Cons 2 (Cons 3 Nil))) == 123

-- fromDigits (Cons 1 (Cons 2 (Cons 3 Nil))) == 123
---Respuesta problema 1
fromAdd (resp, a) x = (((x * a ) + resp), (a * 10))
fromDigits lis = fst (fold fromAdd (0, 1) lis)


-- Problema #2
-- Utilize la funcion fold para implementar
-- la funcion "minMax". Esta funcion acepta
-- una lista de numeros y retorna una pareja
-- ordenada con el minimo y el maximo de esa lista.
--
-- Ejemplo
-- minMax (Cons 5 (Cons 2 (Cons 1 (Cons 10 (Cons 8 Nil))))) == (1, 10)
---Respuesta problema 2
minMaxAuxiliar inicio x = 
    if x > (snd (inicio))
        then  (fst (inicio), x)
        else 
            if x < (fst (inicio))
                then (x, snd (inicio))
                else inicio
minMaxAux x xs = fold minMaxAuxiliar (x, x) xs 



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
---Respuesta problema 3
mmbagg :: (Int -> Int -> Bool) -> (Int, Int) -> Int -> (Int, Int)
mmbagg fun (a, b) x = 
    (if a >= x 
        then x else a, 
        if fun b x 
            then b 
            else x)

minMaxBy f (Cons x xs) = fold (mmbagg f) (x, x) xs
