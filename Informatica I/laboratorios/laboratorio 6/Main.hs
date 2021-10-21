{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Bool(..), Int, Show, undefined)

data Lista a = Cons a (Lista a) | Nil deriving Show

fold fCons fNil Nil = fNil
fold fCons fNil (Cons x xs) = fCons x (fold fCons fNil xs)

mapAcc f x estado = Cons (f x) estado

map f xs = fold (mapAcc f) Nil xs

-- Problema #1
-- Utilize la funcion "fold" para implementar
-- la funcion "fromDigits". Esta funcion debe
-- aceptar una lista con numeros del 0 al 9 y
-- producir como resultado el numero representado
-- por los digitos de esa lista
--
-- Ejemplo:
-- fromDigits (Cons 1 (Cons 2 (Cons 3 Nil))) == 123

fromDigits :: Lista Int -> Int
fromDigits = undefined

-- Problema #2
-- Utilize la funcion fold para implementar
-- la funcion "minMax". Esta funcion acepta
-- una lista de numeros y retorna una pareja
-- ordenada con el minimo y el maximo de esa lista.
--
-- Ejemplo
-- minMax (Cons 5 (Cons 2 (Cons 1 (Cons 10 (Cons 8 Nil))))) == (1, 10)

minMax :: Lista Int -> (Int, Int)
minMax = undefined

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
