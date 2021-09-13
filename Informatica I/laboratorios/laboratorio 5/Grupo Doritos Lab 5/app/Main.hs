{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Float, Bool(..), Show, (+), (-), (==), (||), (*), (>), (&&), (**), floor, fst, snd, div, mod, undefined)

data Lista = Nil | Cons Int Lista deriving Show

fold :: (estado -> Int -> estado) -> estado -> Lista -> estado
fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x
-- fold (+) 0 (Cons 10 (Cons 2 Nil))
-- Problema 1: 
-- A continuacion se le presenta la funcion
-- "pushBack" definida utilizando fold.
-- Su tarea es hacer la reduccion de:
-- "pushBack 3 (Cons 1 (Cons 2 Nil))"

--pushBack x xs
--pushBack 3 (Cons 1 (Cons 2 Nil))
--fold pushAgregador (Cons 3 Nil) (Cons 1(Cons 2 Nil) 
-- | agg: pushAgregador; cero: Cons 3 Nil ; x: Cons 1 xs: Cons 2 Nil
-- pushAgregador (fold pushAgregador (Cons 3 Nil) Cons 2 Nil) Cons 1
-- | agg: puhsAgregador; cero: Cons 3 Nil ; x: Cons 2; xs: Nil
-- pushAgregador pushAgregador (fold pushAgregador (Cons 3 Nil) Nil) Cons 2 Cons 1
-- | agg: pushAgregador; cero: Cons 3 Nil; Nil 
-- pushAgregador pushAgregador Cons 3 Nil Cons 2 Cons 1
-- | estado: pushAgregador Cons 3 Nil Cons 2; x: Cons 1
-- Cons 1 pushAgregador Cons 3 Nil Cons 2
-- | Cons 1 | estado: Cons 3 Nil; x: Cons 2
-- Cons 1 Cons 2 Cons 3 Nil
--
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
rev Nol = Nil
rev (Cons n ns) = pushBack x (rev ns)

takea (r, 0) ns = (r, 0)
takea (r, n) ns = (pushBack xs r, n-1)

take2 n ns = fold takea (Nil, n) xs

take :: Int -> Lista -> Lista 
take 0 Nil = Nil
take n Nil = Nil
take n ns = fst (take2 n (rev xs))
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
elem2 (Cons x Nil) = x

elemtagg (r, n) x = 
  if n == 0 then 
    (Cons x Nil, n -1)
    else 
      (r, n-1)

elemenaux n ns = fold (elemenagg) (Nil, n) ns 


elem n ns = elem2 (fst (elementaux n (rev ns)))

-- Problema 4:
-- Utilizar la funcion "fold" para definir
-- la funcion "update". Esta funcion debe
-- tomar un numero "i", un valor "v" y una
-- lista. Debe retornar una lista donde
-- el elemento en la posicion "i" es
-- reemplazado por "v". POr ejemplo:
-- update 0 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 42 (Cons 2 (Cons 3 Nil)))
-- update 2 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 1 (Cons 2 (Cons 42 Nil)))

update [] [] Nil = Nil

update i v ns = mapu (\h ->
    if h == i
    then v
    else h) ns

-- Problema 5:
-- Utilizar la funcion "fold" para definir
-- la funcion "map". En otras palabras,
-- provea una definicion alterna de la
-- funcion "map" que este definida en
-- terminos de la funcion fold.

mapu [] Nil = Nil
map x (Cons n ns) = Cons (x n) (map x ns)

map x estado n ns = (Cons x ns, estado x ns)

main = undefined 