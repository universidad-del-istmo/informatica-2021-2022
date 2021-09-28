{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (undefined, Float, Int, abs, snd, pi, fst, sqrt, (<), sin, cos, pi, exp, log, asin, acos, atan, sinh, cosh, asinh, acosh, atanh, (<=), (/), (+), (^), (*), (-), div, mod, (==), (/=), (>), (>=), even, (&&), (||), Bool (False, True), Show)

data Lista = Cons Int Lista | Nil deriving Show

fold :: (estado -> Int -> estado) -> estado -> Lista -> estado
fold cons nil Nil = nil
fold cons nil (Cons x xs) = cons (fold cons nil xs) x  

-- Problema 1: 
-- A continuacion se le presenta la funcion
-- "pushBack" definida utilizando fold.
-- Su tarea es hacer la reduccion de:
-- "pushBack 3 (Cons 1 (Cons 2 Nil))"

pushAgregador estado x = Cons x estado
pushBack x xs = fold pushAgregador (Cons x Nil) xs
-- pushBack 3 (Cons 1 (Cons 2 Nil))
-- |pushBack: x = 3, xs = (Cons 1 (Cons 2 Nil))|
-- = fold pushAgregador (Cons 3 Nil) (Cons 1 (Cons 2 Nil))
-- |fold: cons = pushAgregador, nil = (Cons 3 Nil), x = 1, xs = (Cons 2 Nil)
-- = pushAgregador (fold PushAgregador (Cons 3 Nil) (Cons 2 Nil)) 1
-- |fold: cons = pushAgregador, nil = Cons 3 Nil, x = 2, xs = Nil|
-- = pushAgregador (pushAgregador (fold pushAgregador (Cons 3 Nil) Nil) 2) 1 
-- |fold: cons = pushAgregador, nil = (Cons 3 Nil)|
-- = pushAgregador (pushAgregador (Cons 3 Nil) 2) 1
-- |pushAgregador: estado = Cons 3 Nil, x = 2|
-- = pushAgregador (Cons 2 (Cons 3 Nil)) 1
-- |pushAgregador: estado = (Cons 2 (Cons 3 Nil)), x = 1|
-- = Cons 1 (Cons 2 (Cons 3 Nil))

-- Problema 2:
-- Utilizar la funcion "fold" para definir
-- la funcion "take". La funcion "take" debe
-- tomar una lista y un numero "n" y extraer
-- de la lista los primeros "n" elementos
-- que aparecen en la lista.
-- Puede basarse en la funcion "drop" para
-- su implementacio
take _ Nil = Nil
take 0 _ = Nil
take n (Cons x xs) = Cons x (take (n - 1) xs)

takeAgregador estado x = undefined 
 
take' n xs = fold takeAgregador Nil xs

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

elem n Nil = 0
elem 0 (Cons x xs) = x
elem n (Cons x xs) = elem (n - 1) xs

elemAgregador estado x = undefined 
 
elem' n xs = fold elemAgregador 0 xs

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
