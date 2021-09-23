{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Show, (<=), (>=), (-), (!!), fst, undefined)
import Data.List.NonEmpty (cons, reverse)

data Lista = Nil | Cons Int Lista deriving Show

fold :: (estado -> Int -> estado) -> estado -> Lista -> estado
fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x




-- Problema 1: 
-- A continuacion se le presenta la funcion
-- "pushBack" definida utilizando fold.
-- Su tarea es hacer la reduccion de:
-- "pushBack 3 (Cons 1 (Cons 2 Nil))"

pushAgregador estado x = Cons x estado

pushBack x xs =
    fold pushAgregador (Cons x Nil) xs

--respuesta probleam 1 -----
--pushBack 3 (Cons 1 (Cons 2 Nil))
-- = fold pushAgregador (Cons 3 Nil) (Cons 1 (Cons 2 Nil))
-- | agg = pushAgregador, Cero = (Cons 3 Nil),  x = 1, xs = (Cons 2 Nil)
-- = agg (fold agg cero xs) x
-- =Cons 1 (fold agg cero xs)
-- | x = 2, xs = Nil
-- = Cons 1 ( agg (fold agg Cero xs) x)
-- = Cons 1 (Cons 2 (fold agg Cero Nil))
-- = Cons 1 (Cons 2 (Cons 3 Nil))

-- Problema 2:
-- Utilizar la funcion "fold" para definir
-- la funcion "take". La funcion "take" debe
-- tomar una lista y un numero "n" y extraer
-- de la lista los primeros "n" elementos
-- que aparecen en la lista.
-- Puede basarse en la funcion "drop" para
-- su implementacion.


take _ Nil = Nil
take 0 _ = Nil
take n (Cons x xs) = Cons x (take (n - 1) xs)

takeAgg (resultado, 0) x = (resultado, 0)
takeAgg (resultado, n) x = (pushBack x resultado, n - 1)

takeAux n xs = fold takeAgg (Nil, n) xs

take1 n xs = fst (takeAux n (reverse xs))




-- Problema 3:
-- Utilizar la funcion "fold" para definir
-- la funcion "elem". La funcion "elem" toma
-- un numero "i" como parametro y una lista.
-- Esta funcion debe retornar el elemento
-- ubicado en la posicion "i" de la lista.
-- Por ejemplo:
-- elem 2 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) == 3
-- elem 0 (Cons 1 (Cons 2 (Cons 3 Nil))) == 1
-- elem 1 (Cons 1 (Cons 2 (Cons 3 Nil))) == 2


elem :: Int -> [Int] -> Int 
elem n (x:xs) = (xs !! n) - 1


-- Problema 4:
-- Utilizar la funcion "fold" para definir
-- la funcion "update". Esta funcion debe
-- tomar un numero "i", un valor "v" y una
-- lista. Debe retornar una lista donde
-- el elemento en la posicion "i" es
-- reemplazado por "v". POr ejemplo:
-- update 0 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 42 (Cons 2 (Cons 3 Nil)))
-- update 2 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 1 (Cons 2 (Cons 42 Nil)))





-- Problema 5:
-- Utilizar la funcion "fold" para definir
-- la funcion "map". En otras palabras,
-- provea una definicion alterna de la
-- funcion "map" que este definida en
-- terminos de la funcion fold.







main = undefined

