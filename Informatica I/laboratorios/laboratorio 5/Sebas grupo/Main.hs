{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Show, fst, (->), (=),(+), (-), (*), mod, div, (||), (&&), (<), (>), (==), undefined)

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

-- Problema 2:
-- Utilizar la funcion "fold" para definir
-- la funcion "take". La funcion "take" debe
-- tomar una lista y un numero "n" y extraer
-- de la lista los primeros "n" elementos
-- que aparecen en la lista.
-- Puede basarse en la funcion "drop" para
-- su implementacion.

take :: Int -> [a] -> [a]
take n xs = foldr step (Cons [a]) xs n

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

map :: (a -> b) -> [a] -> [b]
map f [a, b] = [a, b]
map f (x:xs) = foldr (x xs -> (f x) : xs)


-- Ejercicios de repaso:
-- A continuacion se proveen ejercicios
-- opcionales que puede elaborar para
-- estudiar para su examen parcial:

-- (1)
-- Definir las funciones "maximo comun divisor"
-- y "minimo comun multiplo" utilizando Haskell.

-- (2)
-- Definir la funcion "raiz cuadrada" utilizando
-- Haskell. Esta version de raiz cuadrada no
-- debe ser exacta, solo debe retornar el numero
-- entero mas cercano (pero menor) a la raiz
-- cuadrada del numero. No utilize las funciones
-- incluidas en Haskell como "floor" y "sqrt"
-- en su definicion. Ejemplo:
--
-- raiz 9 == 3
-- raiz 10 == 3
-- raiz 8 == 2
-- raiz 2 == 1

-- (3)
-- Definir la funcion "convertirALista" utilizando
-- Haskell. Esta funcion toma un numero entero
-- y produce una lista donde cada elemento de la misma
-- es uno de los digitos del numero. Por ejemplo:
--
-- convertirALista 42 == Cons 4 (Cons 2 Nil)
-- convertirALista 712 == Cons 7 (Cons 1 (Cons 2 Nil))

-- (4)
-- Definir la funcion "convertirANumero" utilizando
-- Haskell. Esta funcion toma una lista de numeros
-- como parametro que representan los digitos de
-- un numero. Debe producir como resultado el
-- numero correspondiente. Intente definir
-- esta funcion utilizando "fold". Por ejemplo:

-- convertirANumero (Cons 4 (Cons 2 Nil)) == 42
-- convertirANumero (Cons 7 (Cons 1 (Cons 2 Nil))) == 712

-- (5)
-- Utilize la funcion "fold" para definir la funcion
-- "filter". La funcion "filter" toma una funcion
-- como parametro que representa una condicion y
-- produce una nueva lista con solamente los elementos
-- de la lista original que cumplen esa condicion.
-- Por ejemplo:
--
-- esMayorQue5 x = x > 5
-- filter esMayorQue5 (Cons 4 (Cons 5 (Cons 6 (Cons 7 Nil)))) == Cons 6 (Cons 7 Nil)

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

--EXAMEN PARCIAL 2: 

--pregunta 2
--Implemente en Haskell la funcion minimo comun multiplo de tal forma que dicha funcion acepta tres 
--numeros diferentes y retorna el minimo comun multiplo de esos tres numeros. El minimo comun multiplo es
--el numero mas puequeño que simultaneamente es un multiplo de los tres numeros que se dieron como parametro.

--Ejemplo:
--mcm3 2 5 7 == 70
--mcm3 3 10 15 == 30
--Adicionalmente, elabore la reduccion de esta funcion para "mcm 2 3 4". 
--Por brevedad, solo reduzca 3 recursiones.

mcmAux n m i d =

 if i < n || i < m || i < o
 then d
 else if mod i n == 0 && mod i m == 0 && mod i o == 0
 then mcmAux n m o (i - 1) i
 else mcmAux n m o (i - 1) d

mcm n m o = mcmAux n m o (n * m * o) (n * m * o)



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

--Dada la siguiente funcion "mcm" que acepta dos numeros y produce 
--el minimo comun multiplo de los mismos:

mcmAux2 n m i d =

 if i < n || i < m
 then d
 else if mod i n == 0 && mod i m == 0
 then mcmAux2 n m (i - 1) i
 else mcmAux2 n m (i - 1) d


mcm2 n m = mcmAux2 n m (n * m) (n * m)

--Utilize esta funcion en conjunto con la funcion "fold" estudiada en clase para
--implementar nuevamente la funcion minimo comun multiplo para listas.

fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x

mcmAgregador estado x = mcm2 x estado 

mcmfold xs = fold mcmAgregador 1 xs



main = undefined