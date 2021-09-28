{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Show, fst, snd, Float,  undefined, (-), (+), (*), (==), (>))

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


-- Reduccion pushBack 3 (Cons 1 (Cons 2 Nil))
-- |pushBack: x = 3 / xs = (Cons 1 (Cons 2 Nil)
-- = fold pushAgregador (Cons 3 Nil) (Cons 1 (Cons 2 Nil)
-- |fold : agg = pushAgregador / cero = (Cons 3 Nil) / x = 1 / xs = Cons 2 Nil 
-- = pushAgregador (fold pushAgregador (Cons 3 Nil) (Cons 2 Nil))1
-- | agg = pushAgregador / cero = (Cons 3 Nil) / x = 2 / xs = Nil
-- = pushAgregador [pushAgregador (fold pushAgregador (Cons 3 Nil) Nil ) 2 ] 1 
-- | agg = pushAgregador / cero = Cons 3 Nil 
-- = pushAgregador [pushAgregador (Cons 3 Nil) 2 ] 1

-- | pushAgregador : estado = Cons 3 Nil / x = 2
-- = pushAgregador (Cons 2 (Cons 3 Nil)) 1
-- | estado = (Cons 2 (Cons 3 Nil) / x = 1
-- = Cons 1 (Cons 2 (Cons 3 Nil)
--Resultado : Cons 1 (Cons 2 (Cons 3 Nil )) 


-- Problema 2:
-- Utilizar la funcion "fold" para definir
-- la funcion "take". La funcion "take" debe
-- tomar una lista y un numero "n" y extraer
-- de la lista los primeros "n" elementos
-- que aparecen en la lista.
-- Puede basarse en la funcion "drop" para
-- su implementacion.

reverseAgregador estado x = pushBack x estado
reverse' xs = fold reverseAgregador Nil xs
reverse Nil = Nil
reverse (Cons x xs) = pushBack x (reverse xs)

takeAgregador (resultado, 0) xs = (resultado, 0)
takeAgregador (resultado, x) xs = (pushBack xs resultado, x-1)

takeAux x xs = fold takeAgregador (Nil, x) xs

take 0 Nil = Nil 
take n Nil = Nil 
take n xs = fst (takeAux n (reverse xs))


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

elementoAgregador (resultado, a) x = 
    if a == 0 then 
        (Cons x Nil, a - 1)
        else (resultado, a-1)

elementoAux a xs = fold (elementoAgregador) (Nil, a) xs

elemento (Cons x Nil) = x 
elem a xs = elemento (fst (elementoAux a (reverse xs)))


-- Problema 4:
-- Utilizar la funcion "fold" para definir
-- la funcion "update". Esta funcion debe
-- tomar un numero "i", un valor "v" y una
-- lista. Debe retornar una lista donde
-- el elemento en la posicion "i" es
-- reemplazado por "v". POr ejemplo:
-- update 0 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 42 (Cons 2 (Cons 3 Nil)))
-- update 2 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 1 (Cons 2 (Cons 42 Nil)))

update _ _ Nil = Nil 
update i v xs = map(\z ->
    if z == i
        then v
        else z) xs

-- Problema 5:
-- Utilizar la funcion "fold" para definir
-- la funcion "map". En otras palabras,
-- provea una definicion alterna de la
-- funcion "map" que este definida en
-- terminos de la funcion fold.

map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

map2 f estado x xs = (Cons f x , estado f xs)
fold' map2 estado x xs = map2 (fold map2 estado xs)x

main=undefined