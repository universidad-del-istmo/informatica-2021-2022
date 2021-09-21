{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Float, Bool(..), Show, (+), (-), (==), (||), (*), (>), (&&), (**), floor, fst, snd, div, mod, undefined)

--Entrega de examen Parcial 2

--Pregunta 1: Implemente la funcion "maximo comun divisor" de tal forma que dicha funcion acepta tres numeros diferentes 
--y retorna el maximo comun divisor de esos tres numeros. El maximo comun divisor es el numero mas grande que puede dividir 
--de forma exacata a los tres numeros que se dio como parametro.

--Ejemplo:

--mcd3 20 25 30 == 5
--mcd3 11 13 17 == 1

--Adicionalmente, elabore la reduccion de esta funcion para "mcd 2 4 6". Para brevedad, solo reduzca tres recursiones.

--Respuesta Pregunta 1:

--Código:



mcd : : Int -> Int -> Int -> Int

mcd x y z = mcd' x y z 1 1 1



mcd' x y z i d =

 if x == 1 || y == 1 || z==1

 then d

 else if mod x (i + 1) == 0 && mod y (i + 1) == 0 && mod z (i + 1) == 0

 then mcd' x y z (i + 1) ( i +1)

 else mcd' x y z (i + 1) d


--Reducción:

--mcd 2 4 6

--x == 1 || y == 1 || z == 1

--x = 2, y = 4, z =6

--mod x (i + 1) && mod y (i + 1) == 0 && mod z (i + 1) == 0

--mod 2 (i +1) == 0 && mod 4 (i + 1) == 0 && mod 6 (i + 1) == 0



--Pregunta 3: Implemente en Haskell la funcion maximo comun divisor de tal forma que dicha funcion acepte una lista de numeros 
--y retorne el maximo comun divisor de los numeros en dicha lista. El maximo comun divisor es el numero mas grande que 
--puede dividir de forma exacata a todos los numeros de la lista que se dio como parametro.



--Ejemplo
--mcdL (Cons 20 (Cons 25 (Cons 30 Nil))) == 5
--mcdL (Cons 11 (Cons 13 (Cons 17 Nil))) == 1 


--Respuesta Pregunta 2:

data Lista = Cons Int Lista | Nil deriving show

contar : : Lista -> Int

contar Nil = 0

contar (Cons x xs) 1 + contar xs



mcdL : : Lista -> Int

mcdLAux x xs i d

x 1 = 1 || xs 1 = 1

then d

else if mod x (i +1) == 0 && mod xs (i+1) == 0

then mcdLAux x xs (i + 1)(i + 1)

else mcdLAux x xs (i + 1)d


--Pregunta 5: Dada la siguiente funcion "mcd" que acepta dos numeros y produce el maximo comun divisor de los mismos:

--mcdAux n m i d =

 --if n == i || m == i

 --then d

 --else if mod n (i + 1) == 0 && mod m (i + 1) == 0

 --then mcdAux n m (i + 1) (i + 1)

 --else mcdAux n m (i + 1) d



--mcd :: Int -> Int -> Int

--mcd n m = mcdAux n m 1 1



--Utilize esta funcion en conjunto con la funcion "fold" estudiada en clase para implementar nuevamente la funcion maximo comun divisor para listas.


--Respuesta a Pregunta 5:

fold :: (estado -> Int -> Int -> estado ) -> estado -> Int -> estado

mcdAux n m i d =

fold agg cero n == i || m == i = d

fold agg cero mod n (fold agg cero i +1) == 0 && mod m (fold agg cero i + 1) == 0

fold agg cero mcdAux nm (foldd agg cero i +1)(fold agg cero i +1)

fold agg cero mcdAux nm (fold agg cero i + 1)d
