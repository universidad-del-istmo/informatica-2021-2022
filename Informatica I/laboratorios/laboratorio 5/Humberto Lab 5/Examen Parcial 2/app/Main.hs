{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Show, Float , Bool (..) , lcm, (+), (-), mod, (==), floor, (^),  (||), (*), (<), (>), (&&), (**), fst, snd,  undefined, div)

data Lista = Nil | Cons Int Lista deriving Show




--Problema 2

--Implemente en Haskell la funcion minimo comun
-- multiplo de tal forma que dicha funcion acepta 
--tres numeros diferentes y retorna el minimo
-- comun multiplo de esos tres numeros. El minimo
-- comun multiplo es el numero mas puequeño que simultaneamente 
--es un multiplo de los tres numeros que se dieron como 
--parametro.



--mcm3 2 5 7 == 70

--mcm3 3 10 15 == 30



--Adicionalmente, elabore la reduccion de esta funcion para "mcm 2 3 4". Por brevedad, solo reduzca 3 recursiones.

--Su respuesta
mcmDos n m l q p =



 if q < n || q < m || q < l then p



 else if mod q n == 0 && mod q m == 0 && mod q l == 0



 then mcmDos n m l (q - 1) q



 else mcmDos n m l (q - 1) p



mcm n m l= mcmDos n m l multiplicaciónDe3Numeros multiplicaciónDe3Numeros

 where

     multiplicaciónDe3Numeros = n * m * l





--reducción mcm 2 3 4

-- mcmDos 2 3 5 (2* 3 * 4)(2 * 3* 4)

-- | mcmDos n m l q p =

-- | mcmDos n = 2, m = 3, l = 5  p = 12

-- | if q < n || q < m || q < l then p

-- = if q < 2 || q < 3 || q < 1 then 12

-- | else if mod q n == 0 && mod q m == 0 && mod q l == 0 [n/2, m/3, l/5]

-- = else if mod q 2 == 0 && mod q 3 == 0 && mod q 5 == 0





--problema 4

--Implemente en Haskell la funcion minimo comun multiplo de 
--tal forma que dicha funcion acepte una lista de numeros y 
--retorne el minimo comun multiplo de dicha lista. El minimo 
--comun multiplo es el numero mas puequeño que simultaneamente 
--es un multiplo de todos los numeros en la lista que se dio 
--como parametro.



--Ejemplo:



--mcmL (Cons 2 (Cons 5 (Cons 7 Nil))) == 70

--mcmL (Cons 3 (Cons 10 (Cons 15 Nil))) == 30



mcmL1 Nil m = m
mcmL1 (Cons x xs) m = mcmL1 xs (mcm1 x m)















--Problema 6

--Dada la siguiente funcion "mcm" 
--que acepta dos numeros y produce el minimo 
--comun multiplo de los mismos:

mcmAux n m i d =

 if i < n || i < m

 then d

 else if mod i n == 0 && mod i m == 0

 then mcmAux n m (i - 1) i

 else mcmAux n m (i - 1) d



mcm1 n m = mcmAux n m (n * m) (n * m)




--Utilize esta funcion en conjunto con --
--la funcion "fold" estudiada en clase para implementar 
--nuevamente la funcion minimo comun multiplo para listas.




fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x

mcmAgregador estado x = mcm1 x estado

mcmfold = fold mcmAgregador 1


main= undefined
