{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude (Int, undefined, IO, Show, (+), (*), (-), fst, div, mod, (==), (||), (&&), (**))

data Lista = Nil | Cons Int Lista deriving Show 


fold :: (estado -> Int -> estado) -> estado -> Lista -> estado
fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x
    

pushBack x xs =
    fold pushAgregador (Cons x Nil) xs

pushAgregador estado x = Cons x estado

pushBack' x xs =
    fold pushAgregador (Cons x Nil) xs

reverseAgregador estado x = pushBack' x estado

reverse' xs = fold reverseAgregador Nil xs


--Pregunta 1 
--Implemente la funcion "maximo comun divisor" de tal forma que dicha funcion
-- acepta tres numeros diferentes y retorna el maximo comun divisor de esos tres numeros. 
--El maximo comun divisor es el numero mas grande que puede 
--dividir de forma exacata a los tres numeros que se dio como parametro.
--Ejemplo:
--mcd3 20 25 30 == 5
--mcd3 11 13 17 == 1
--Adicionalmente, elabore la reduccion de esta funcion para "mcd 2 4 6". Para brevedad, solo reduzca tres recursiones.

mcd3 :: Int -> Int -> Int -> Int

mcd3 x z y = mcdSec x z y 0 0 0

mcdSec x z y a b c =

 if x == a || z == a || y == a 

 then b

 else if 
     mod x (a + 1) == 0 && 
     mod z (a + 1) == 0 && 
     mod y (a + 1) == 0 
 then k
 else t

    where 
        k = mcdSec x z y (a + 1 ) (a + 1 ) (a + 1 )  
        t = mcdSec x z y (a + 1) b c                   
 
--Reducción de mcd 2 4 6 
--mcd3 2 4 6 
-- = mcdSec 2 4 6 0 0 0 
-- |x = 2, z = 4, y = 6, a = 1, b = 1, c = 1
-- = if x == a || z == a || y == a 
-- = mod 2 (0 + 1) == 0 && mod 4 (0 + 1) == 0 && mod 6 (0 + 1) == 0 = true

--mcd3 2 4 6 
-- = mcdSec 2 4 6 1 1 1 
-- |x = 2, z = 4, y = 6, a = 1, b = 1, c = 1
-- = if x == a || z == a || y == a 
-- = mod 2 (1 + 1) == 0 && mod 4 (1 + 1) == 0 && mod 6 (1 + 1) == 0 = true

-- = mcdSec 2 4 6 2 2 2 
-- |x = 2, z = 4, y = 6, a = 2, b = 2, c = 2
-- = if x == a || z == a || y == a = true
-- = 2

        
--Pregunta 3
--Implemente en Haskell la funcion maximo comun divisor de tal forma que 
--dicha funcion acepte una lista de numeros y retorne el maximo comun 
--divisor de los numeros en dicha lista. El maximo comun divisor es 
--el numero mas grande que puede dividir de forma exacata a todos 
--los numeros de la lista que se dio como parametro.

--Ejemplo

--mcdL (Cons 20 (Cons 25 (Cons 30 Nil))) == 5
--mcdL (Cons 11 (Cons 13 (Cons 17 Nil))) == 1

--mcdL Nil = 0  
--mcdL (Cons x xs) = mcdL' (Cons x xs) 1 1 

--mcdL' (Cons x (Cons y xs)) i d = 
   -- if x == i || y == i 
   --     then d
    --    else  if 
    --        mod x (i + 1) == 0 && 
     --       mod y (i + 1) == 0
      --      then mcdL' (Cons x xs) (i + 1) (i + 1) 
      --      else mcdL' (Cons x xs) (i + 1) d 

mcd'' Nil n = n
mcd'' (Cons x xs) n = mcd'' xs (mcd x n) 

mcdL (Cons x xs) = mcd'' (Cons x xs) 0 


-- Pregunta 5 
-- Dada la siguiente funcion "mcd" que acepta dos numeros y produce el maximo 
-- comun divisor de los mismos:



mcdAux n m i d =

 if n == i || m == i

then d

 else if mod n (i + 1) == 0 && mod m (i + 1) == 0

 then mcdAux n m (i + 1) (i + 1)

 else mcdAux n m (i + 1) d
 

mcd :: Int -> Int -> Int

mcd n m = mcdAux n m 1 1

--Utilize esta funcion en conjunto con la funcion "fold" estudiada en clase para implementar 
--nuevamente la funcion maximo comun divisor para listas.


mcdAgregador estado x = mcd x estado 

mcd' xs = fold mcdAgregador 0 xs

--fold agg cero Nil = cero
--fold agg cero (Cons x xs) =
  --  agg (fold agg cero xs) x




main :: IO ()
main = undefined