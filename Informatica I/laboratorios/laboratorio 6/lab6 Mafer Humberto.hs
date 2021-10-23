{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Bool(..), (*), fst, snd, (+), (<), (>=), (<=), (>), Int, Show, undefined)

data Lista a = Cons a (Lista a) | Nil deriving Show

foldL fCons fNil Nil = fNil
foldL fCons fNil (Cons x xs) = fCons x (foldL fCons fNil xs)

mapAcc f x estado = Cons (f x) estado

map f xs = fold (mapAcc f) Nil xs

fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x
pushAgregador estado x = Cons x estado

pushBack x xs =
    fold pushAgregador (Cons x Nil) xs
reverseAgregador estado x = pushBack x estado
reverse' xs = fold reverseAgregador Nil xs
reverse Nil = Nil
reverse (Cons x xs) = pushBack x (reverse xs)
-- Problema #1
-- Utilize la funcion "fold" para implementar
-- la funcion "fromDigits". Esta funcion debe
-- aceptar una lista con numeros del 0 al 9 y
-- producir como resultado el numero representado
-- por los digitos de esa lista
--
-- Ejemplo:
-- fromDigits (Cons 1 (Cons 2 (Cons 3 Nil))) == 123


--convertirANumeroAux p Nil = 0 
--convertirAux p (Cons x xs) = x * p + convertirANumeroAux (p *10) xs 
--convertirANumero xs =convertirANumeroAux 1 (reverse xs)

--fromdigit Nil (resultado, n) = 0 
--fromDigitAgg (resultado, n) (Cons x xs) = x * n + fromDigitAgg (resultado, n*10) xs
--fromDigitAuxiliar n (Cons x xs) = fold fromDigitAgg (Nil, n) (Cons x xs)

--fromDigits :: Lista Int -> Int
--fromDigits Nil = 0
--fromDigits (Cons x xs) = fromDigitAuxiliar 1 (reverse (Cons x xs))

fromAdd (respuesta, n) x = (((x * n) + respuesta), (n * 10))
fromDigits Nil = 0
fromDigits (Cons x xs) = fst (fold fromAdd (0,1) (Cons x xs))

-- Problema #2
-- Utilize la funcion fold para implementar
-- la funcion "minMax". Esta funcion acepta
-- una lista de numeros y retorna una pareja
-- ordenada con el minimo y el maximo de esa lista.
--
-- Ejemplo
-- minMax (Cons 5 (Cons 2 (Cons 1 (Cons 10 (Cons 8 Nil))))) == (1, 10)

ifmayorque n m = 
    if n > m 
        then n 
        else m 
minMaxAgg estado x =
    if x > (snd (estado))
    then (fst(estado), x)
    else if x < (fst (estado))
    then (x, snd (estado))
    else estado 

minMaxAux x xs = fold minMaxAgg (x,x) xs

minMax (Cons x xs) = minMaxAux  x xs

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

minMaxByAgg f (a, b) x = 
    (if f a x 
    then x
    else a,
    if f b x 
    then b 
    else x)

minMaxBy :: (a -> a -> Bool) -> Lista a -> (a, a)
minMaxBy f (Cons x xs) = fold (minMaxByAgg f) (x,x) (Cons x xs)

main = undefined
