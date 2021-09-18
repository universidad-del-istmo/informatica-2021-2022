{-#LANGUAGE NoImplicitPrelude #-}


import Prelude (Int, Show, undefined, (+), (-), (*), mod, div, (||), (&&), (<), (>), (==))

-- Respuestas paracial 2 Diego Giron Figueroa preguntas pares --------- 

--pregunta 2
--Implemente en Haskell la funcion minimo comun multiplo de tal forma que dicha funcion acepta tres 
--numeros diferentes y retorna el minimo comun multiplo de esos tres numeros. El minimo comun multiplo es
-- el numero mas puequeño que simultaneamente es un multiplo de los tres numeros que se dieron como parametro.

--Ejemplo:
--mcm3 2 5 7 == 70

--mcm3 3 10 15 == 30



--Adicionalmente, elabore la reduccion de esta funcion para "mcm 2 3 4". Por brevedad, solo reduzca 3 recursiones.
-- respuesta a pregunta 2 
mcmAux1 n m c i d =
 if i < n || i < m || i < c 
 then d
 else if mod i n == 0 && mod i m == 0 && mod i c == 0 
 then mcmAux1 n m c (i - 1) i
 else mcmAux1 n m c (i - 1) d

mcm1 n m c = mcmAux1 n m c (n * m * c)(n * m * c)

-- reduccion
-- mcm 2 3 4
--mcmAux1 2 3 4 24 24
-- |n = 2 , m = 3, c = 4, i = 24, d = 24
-- = if 24 < 2 || 24 < 3 || 24 < 4 = False 
-- if mod 24 2 == 0 && mod 24 3 == 0 && mod 24 2 == 0 = True
-- mcmAux1 2 3 4 23 24 
-- | n = 2, m = 3, c = 4, i = 23,  d = 24
-- = if 23 < 2 || 23 < 3 || 23 < 4 = False 
-- = if mod 23 2 == 0 && mod 23 3 == 0 && mod 23 4 == 0 = False 
-- mcmAux1 2 3 4 22 24
-- | n = 2, m = 3, c = 4, i = 22, d = 24 
-- = if = 22 < 2 || 22 < 3 || 22 < 4 = False 
-- = if mod 22 2 == 0 && mod 22 3 == 0 && mod 22 4 == 0 = False 
-- mcmAux1 2 3 4 21 24

--pregunta 4 
--Implemente en Haskell la funcion minimo comun multiplo de tal forma 
--que dicha funcion acepte una lista de numeros y retorne el minimo comun multiplo 
--de dicha lista. El minimo comun multiplo es el numero mas puequeño que simultaneamente es
 --un multiplo de todos los numeros en la lista que se dio como parametro.

--Ejemplo:

--mcmL (Cons 2 (Cons 5 (Cons 7 Nil))) == 70

--mcmL (Cons 3 (Cons 10 (Cons 15 Nil))) == 30

data Lista = Nil | Cons Int Lista deriving Show 

mcmLaux Nil a = a
mcmLaux (Cons x xs) a = mcmLaux xs (mcm2 x a)
mcmL li = mcmLaux li 1


--mcmL (Cons x xs) = mcmLaux (Cons x xs) a

-- pregunta 6 

--Dada la siguiente funcion "mcm" que acepta dos numeros y produce el minimo comun multiplo de los mismos:



mcmAux2 n m i d =

 if i < n || i < m

 then d

 else if mod i n == 0 && mod i m == 0

 then mcmAux2 n m (i - 1) i

 else mcmAux2 n m (i - 1) d



mcm2 n m = mcmAux2 n m (n * m) (n * m)



--Utilize esta funcion en conjunto con la funcion "fold" estudiada en clase para
-- implementar nuevamente la funcion minimo comun multiplo para listas.


fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x

mcmAgregador estado x = mcm2 x estado 

mcmfold xs = fold mcmAgregador 1 xs


-- preguntas impares 1, 3, 5 Jorge Ferguson ---------------------




mcdAux n m i d =

 if n == i || m == i

 then d

 else if mod n (i + 1) == 0 && mod m (i + 1) == 0

 then mcdAux n m (i + 1) (i + 1)

 else mcdAux n m (i + 1) d

mcd :: Int -> Int -> Int

mcd n m = mcdAux n m 1 1


--Pregunta 1

mcdAux' x z y a b c =

 if x == a || z == a || y == a 

 then b

 else if 

     mod x (a + 1) == 0 && 

     mod z (a + 1) == 0 && 

     mod y (a + 1) == 0 

 then e

 else f

    where 

        e = mcdAux' x z y (a + 1 ) (a + 1 ) (a + 1 )  

        f = mcdAux' x z y (a + 1) b c

mcd3Nums :: Int -> Int -> Int -> Int

mcd3Nums x z y = mcdAux' x z y 1 1 1


--Pregunta 3

mcdListA Nil n = n 
mcdListA (Cons x xs) n = mcdListA xs (mcd x n)

mcdL (Cons x xs) = mcdListA (Cons x xs) 0


--Pregunta 5


fold1 agg 0 Nil = 0
fold1 agg 0 (Cons x xs) =
    agg (fold1 agg 0 xs) x

mcdAgg estado x = mcd x estado 
mcdfold xs = fold1 mcdAgg 0 xs




main = undefined

