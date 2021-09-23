+{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Show,Float, Bool(..), div, snd, fst, (-),(+), mod, (||), (*),(/),(^), (&&), (>),(==), undefined)

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

-- pushback 3 (Cons 1(Cons 2 Nil))
--fold pushAgregador (Cons 3 Nil) (Cons 1(Cons 2 Nil) 
-- | agg: pushAgregador; cero: Cons 3 Nil ; x: Cons 1 xs: Cons 2 Nil
-- pushAgregador (fold pushAgregador (Cons 3 Nil) Cons 2 Nil) Cons 1
-- | agg: puhsAgregador; cero: Cons 3 Nil ; x: Cons 2; xs: Nil
-- pushAgregador pushAgregador (fold pushAgregador (Cons 3 Nil) Nil) Cons 2 Cons 1
-- | agg: pushAgregador; cero: Cons 3 Nil; Nil / (fold pushAgregador (Cons 3 Nil) Nil = Cons 3 Nil)
-- pushAgregador pushAgregador Cons 3 Nil Cons 2 Cons 1
-- | estado: pushAgregador Cons 3 Nil Cons 2; x: Cons 1
-- Cons 1 pushAgregador Cons 3 Nil Cons 2
-- | Cons 1/ estado: Cons 3 Nil; x: Cons 2
-- Cons 1 Cons 2 Cons 3 Nil
-- = Cons 1(Cons 2(Cons 3 Nil) 

reverseAgregador estado x = pushBack x estado
reverse' xs = fold reverseAgregador Nil xs
reverse Nil = Nil
reverse (Cons x xs) = pushBack x (reverse xs)

-- Problema 2:
-- Utilizar la funcion "fold" para definir
-- la funcion "take". La funcion "take" debe
-- tomar una lista y un numero "n" y extraer
-- de la lista los primeros "n" elementos
-- que aparecen en la lista.
-- Puede basarse en la funcion "drop" para
-- su implementacion.

--takeAgregador (x, 0) c = (Cons c x, 0)
--takeAgregador (result, 0) x = Nil
--takeAgregador (result, n) x = (Cons x result, n -1)  --(Cons x result, n-1) --Cons x (xs, n-1) --takeAgregador (resultado, n-1) --take n xs (takeAgregador )
--takeAgregador (result, n) x = (result, n-1)
-- x 
--takeAux n  xs = fold (takeAgregador) (Nil, n) xs --fold (takeAgregador) (Nil, n-1) xs 
-- 
takeAgregador (resultado, 0) xs = (resultado, 0)
takeAgregador (resultado, n) xs = (pushBack xs resultado, n-1)

takeAux n xs = fold takeAgregador (Nil, n) xs



take 0 Nil = Nil
take n Nil = Nil
take n xs = fst (takeAux n(reverse xs))
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

elemAgregador (resultado, n) x = 
    if n == 0 
        then a 
        else b 

    where 
        a = (Cons x Nil, n - 1)
        b = (resultado, n - 1) 

elemAux n xs = fold (elemAgregador) (Nil, n) xs

elem n xs = elemAgg (fst (elemAux n (reverse xs)))
 

-- Problema 4:
-- Utilizar la funcion "fold" para definir
-- la funcion "update". Esta funcion debe
-- tomar un numero "i", un valor "v" y una
-- lista. Debe retornar una lista donde
-- el elemento en la posicion "i" es
-- reemplazado por "v". POr ejemplo:
-- update 0 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 42 (Cons 2 (Cons 3 Nil)))
-- update 2 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 1 (Cons 2 (Cons 42 Nil)))

--fold agg cero Nil = cero
--fold agg cero (Cons x xs) =
  --  agg (fold agg cero xs) x

--updateAgg (estado i v xs )x = i (Cons x xs)
--updateAux i v Nil = Nil 
--updateAux i v  (Cons x xs) = 
   -- if i == x  
   --    then v fold updateAux 
   -- else undefined

update :: Eq t => t -> t -> [t] -> [t]
update _ _ [] = []
update' i v xs = 
    map (\h -> 
        if h == i 
            then v 
            else h) xs


--elemAgg (Cons x Nil) = x 

--elemAgregador (resultado, n) x = 
   -- if n == 0 
   --     then a 
   --     else b 

   -- where 
        --a = (Cons x Nil, n - 1)
       -- b = (resultado, n - 1) 

--elemAux n xs = fold (elemAgregador) (Nil, n) xs

--elem n xs = elemAgg (fst (elemAux n (reverse xs)))
 
--FUNCIONA


-- Problema 5:
-- Utilizar la funcion "fold" para definir
-- la funcion "map". En otras palabras,
-- provea una definicion alterna de la
-- funcion "map" que este definida en
-- terminos de la funcion fold.


map _ Nil = Nil 
map f (Cons x xs) = Cons (f x) (map f xs) 

map' f estado x xs = (Cons f x, estado f xs)

fold' map' estado x xs = map' (fold map' estado xs) x

--map _ Nil = Nil 
--map f (Cons x xs) = Cons (f x) (map f xs) 

--fold agg cero Nil = cero
--fold agg cero (Cons x xs) =
  --  agg (fold agg cero xs) x
  -- (6)

-- Defina en Haskell la funcion "existenValores". Esta
-- funcion toma un numero "n" y una lista. Debe buscar
-- si en la lista existen 2 numeros que al ser sumados
-- producen como resultado "n". En caso afirmativo,
-- producir True, de lo contrario producir False.
-- Ejemplo:
--
-- existenValores 6 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) == True
-- existenValores 2 (Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))) == False
-- existenValores 2 (Cons (-1) (Cons 2 (Cons 3 (Cons 4 Nil)))) == True

existe v Nil = False
existe v (Cons x xs) = v == x || existe v xs

existenValores n Nil = False
existenValores n (Cons x Nil) = False
existenValores n (Cons x xs) =
    existe (n - x) xs || existenValores n xs

main = undefined