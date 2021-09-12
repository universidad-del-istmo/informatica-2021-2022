{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Float, Bool(..), Show, (+), (-), (==), (||), (*), (>), (&&), (**), floor, fst, snd, div, mod, undefined)

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

-- Procedimiento:
-- "pushBack 3 (Cons 1 (Cons 2 Nil))"
-- SUSTITUCION DE VARIABLES
-- fold pushAgregador (Cons 3 Nil) (Cons 1 (Cons 2 Nil))
-- SUSTITUCION DE VARIABLES
-- push Agregador (fold pushAgregador Cons 3 (Cons 1 (Cons 2 Nil))) Nil
-- SUSTITUCION DE VARIABLES
-- push Agregador (push Agregador (fold push Agregador Cons 2 (Cons 3 (Cons 1 Nil))) 2) Nil)
-- SUSTITUCION DE VARIABLES
-- pushAgregador (pushAgregador Cons 2 (Cons 3 (Cons 1 Nil))) Nil
-- SUSTITUCION DE VARIABLES
-- Cons (Cons 3 (Cons 1) (Cons 2)) Nil
-- SUSTITUCION DE VARIABLES
-- Cons (Cons 1 (Cons 2)) Cons 3) Nil
-- Cons 1 (Cons 2 (Cons 3 NIl))

pushAgregador estado x = Cons x estado
pushBack x xs =
    fold pushAgregador (Cons x Nil) xs

-- Problema 2:
-- Utilizar la funcion "fold" para definir
-- la funcion "take". La funcion "take" debe
-- tomar una lista y un numero "n" y extraer
-- de la lista los primeros "n" elementos
-- que aparecen en la lista.
-- Puede basarse en la funcion "drop" para
-- su implementacion.

reveres Nil = Nil
reverse (Cons x xs) = pushBack x (reveres xs)

takeAgregador (resultado, y) x =
    if y > 0
        then r
        else z
    
    where
        r= ((pushBack x resultado, y-1))
        z= (resultado, y-1)

takeAux y xs = fold (takeAgregador) (Nil, y) xs
takePrimo y xs = (fst (takeAux y (reverse xs)))


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

elemAgg (Cons x Nil) = x

elem Agregador (resultado, y) x =
    if y == 0
        then r 
        else f

    where 
        r = (Cons x Nil, y-1)
        f = (resultado, y-1)

elemAux y xs = fold (elemAgregador) (Nil, y) xs
elem y xs = elem Agg (fst (elemAux y (reverse xs)))


-- Problema 4:
-- Utilizar la funcion "fold" para definir
-- la funcion "update". Esta funcion debe
-- tomar un numero "i", un valor "v" y una
-- lista. Debe retornar una lista donde
-- el elemento en la posicion "i" es
-- reemplazado por "v". POr ejemplo:
-- update 0 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 42 (Cons 2 (Cons 3 Nil)))
-- update 2 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 1 (Cons 2 (Cons 42 Nil)))

--NO LA ENTENDI

-- Problema 5:
-- Utilizar la funcion "fold" para definir
-- la funcion "map". En otras palabras,
-- provea una definicion alterna de la
-- funcion "map" que este definida en
-- terminos de la funcion fold.

map a Nil = Nil
map b (Cons x xs) = COns (f x) (map f xs)

mapPrimo f estado x xs = (Cons f x, estado f xs)
fold mapPrimo estado x xs = mapPrimo (fold mapPrimo estaod xs) xs
