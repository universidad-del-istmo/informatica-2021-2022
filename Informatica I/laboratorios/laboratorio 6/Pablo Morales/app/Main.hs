module Lab6 where
{-# LANGUAGE NoImplicitPrelude #-}
import Prelude (Bool(..), Int, Show, fst, snd, (+), (<), (>=), (<=), (*), (>),(==), (||), undefined)
data Lista a = Cons a (Lista a) | Nil deriving Show

-- Laboratorio No. 6
-- Pablo Morales

foldf :: (Int -> Int -> Int) -> Int -> Lista Int -> Int
foldf fCons fNil Nil = fNil
foldf fCons fNil (Cons x xs) = fCons x (foldf fCons fNil xs)

mapAcc f x estado = Cons (f x) estado
map f xs = fold (mapAcc f) Nil xs


fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x



-- Problema #1
-- Utilice la función "fold" para implementar
-- la función "fromDigits". Esta función debe
-- aceptar una lista con números del 0 al 9 y
-- producir como resultado el número representado
-- por los dígitos de esa lista.
--
-- Ejemplo:
-- fromDigits (Cons 1 (Cons 2 (Cons 3 Nil))) == 123
-- fromDigits :: Lista Int -> Int
-- fromDigits = undefined


fromDigits :: Lista Int -> Int
fromDigits (Cons x xs) = fst (fold fromDigitsAgregador  (0, 1) (Cons x xs))


fromDigitsAgregador (x', x) r =
     ((r * x) + x', x * 10) 



-- Problema #2
-- Utilize la funcion fold para implementar
-- la funcion "minMax". Esta funcion acepta
-- una lista de numeros y retorna una pareja
-- ordenada con el minimo y el maximo de esa lista.
--
-- Ejemplo
-- minMax (Cons 5 (Cons 2 (Cons 1 (Cons 10 (Cons 8 Nil))))) == (1, 10)

-- minMax :: Lista Int -> (Int, Int)
-- minMax = undefined

minMax :: Lista Int -> (Int, Int)
minMax (Cons x xs) = minMaxAux x xs

-- minMaxAgregador :: GHC.Classes.Ord b => (b, b) -> b -> (b, b)
minMaxAgregador estado x =
    if x > (fst (estado))
        then  (x, snd (estado))
        else   if x < (snd (estado))
                then (fst (estado, x))
                else estado


minMaxAux x  xs = fold minMaxAgregador (x,  x) xs 


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

minMaxBy :: (a -> a -> Bool) -> Lista a -> (a, a)
minMaxBy = undefined

main = undefined
