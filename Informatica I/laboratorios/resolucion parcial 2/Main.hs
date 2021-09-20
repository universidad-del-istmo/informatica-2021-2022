module Main where

{-#LANGUAGE NoImplicitPrelude #-}

import Prelude (Int, Show, undefined, mod, div, (||), , (>), (==), (+), (-), (*), (&&), (<))

data Lista = Cons Int Lista | Nil deriving Show

-- Pregunta 2
-- Implemente en Haskell la funcion minimo comun multiplo de tal forma que dicha funcion 
-- acepta tres numeros diferentes y retorna el minimo comun multiplo de esos tres numeros. 
-- El minimo comun multiplo es el numero mas puequeño que simultaneamente es un multiplo de 
-- los tres numeros que se dieron como parametro.

mcmAux n m c i d =
 if i < n || i < m || i < c
 then d
 else if mod i n == 0 && mod i m == 0 && mod i c == 0
 then mcmAux n m c (i - 1) i
 else mcmAux n m c (i - 1) d

mcm n m c = mcmAux n m c (n * m * c) (n * m * c)

-- mcm 3 4 2
-- |n = 3, m = 4, c = 2|
-- = mcmAux 3 4 2 (3 * 4 * 2) (3 * 4 * 2)
-- = mcmAux 3 4 2 24 24
-- |n = 3, m = 4, c = 2, i = 24, d = 24|
-- Linea 18 especifica que si el residuo entre i y n, m y c es 0 entonces se continúa con la función
-- condicional mcmAux n m c (i - 1) i
-- = mcmAux 3 4 2 (24 - 1) 24
-- = mcmAux 3 4 2 23 24 
-- |n = 3, m = 4, c = 2, i = 23, d = 24|
-- Linea 19 especifica que si el residuo entre i y n, m y c es diferente de 0 se continúa con la función
-- condicional mcmAux n m c (i - 1) d
-- = mcmAux 3 4 2 (23 - 1) 24
-- = mcmAux 3 4 2 22 24
-- |n = 3, m = 4, c = 2, i = 22, d = 24|
-- Linea 19 especifica que si el residuo entre i y n, m y c es diferente de 0 se continúa con la función
-- condicional mcmAux n m c (i - 1) d
-- mcmAux 3 4 2 (22 - 1) 24
-- mcmAux 3 4 2 21 24
-- La función continuará hasta que i sea menor que n, m o c, dando como resultado d


-- La segunda linea específica que si i es menor que cualquiera de los 3 primeros números 
-- se tiene que proceder con el resto de la reducción, si embargo 12 no es menor que cualquiera
-- de los 3 números por lo que respuesta es d, equivaliendo a 12 el mcm.

-- Pregunta 4
-- Implemente en Haskell la funcion minimo comun multiplo de tal forma que dicha funcion acepte una
-- lista de numeros y retorne el minimo comun multiplo de dicha lista. El minimo comun multiplo es el
-- número más pequeño que simultaneamente es un multiplo de todos los numeros en la lista que se dio como parametro.

mcmLAuxiliar Nil n = n
mcmLAuxiliar (Cons x xs) n = mcmLAuxiliar xs (mcm' x n)
mcmL lista = mcmLAuxiliar lista 1

-- Pregunta 6
-- Dada la siguiente funcion "mcm" que acepta dos numeros y produce el minimo comun multiplo de los mismos:

mcmAux' n m i d =
 if i < n || i < m
 then d
 else if mod i n == 0 && mod i m == 0
 then mcmAux' n m (i - 1) i
 else mcmAux' n m (i - 1) d

mcm' n m = mcmAux' n m (n * m) (n * m)

-- Utilize esta funcion en conjunto con la funcion "fold" estudiada en clase para implementar nuevamente la funcion minimo}
-- comun multiplo para listas.

fold agg cero Nil = cero
fold agg cero (Cons x xs) = agg (fold agg cero xs) x

mcmAgregador estado x = mcm' x estado 

mcm'' xs = fold mcmAgregador 1 xs

main = undefined
