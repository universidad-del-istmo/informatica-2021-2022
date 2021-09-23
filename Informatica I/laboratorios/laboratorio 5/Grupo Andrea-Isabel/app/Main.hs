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

pushAgregador estado x = Cons x estado

pushBack x = fold pushAgregador (Cons x Nil)

------------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

-- Problema 2:
-- Utilizar la funcion "fold" para definir
-- la funcion "take". La funcion "take" debe
-- tomar una lista y un numero "n" y extraer
-- de la lista los primeros "n" elementos
-- que aparecen en la lista.

reverse Nil = Nil 
reverse (Cons x xs) = pushBack x (reverse xs)

takeAgregador (resultado, n) x = 
    if n > 0 
        then a
        else y
   where 
       a = ((pushBack x resultado, n -1))
       y = (resultado, n - 1)

takeAux n xs = fold (takeAgregador) (Nil, n) xs 

take' n xs = (fst (takeAux n (reverse xs)))

--------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------

-- Problema 3:
-- Utilizar la funcion "fold" para definir
-- la funcion "elem". La funcion "elem" toma
-- un numero "i" como parametro y una lista.
-- Esta funcion debe retornar el elemento
-- ubicado en la posicion "i" de la lista.
-- Por ejemplo:
-- elem 2 [1,2,3,4] == 3
-- elem 0 [1,2,3] == 1
-- elem 1 [1,2,3] == 2

elemAgg (Cons x Nil) = x 

elemAgregador (resultado, n) x = 
    if n == 0 
        then a 
        else y

    where 
        a = (Cons x Nil, n - 1)
        y = (resultado, n - 1) 

elemAux n xs = fold (elemAgregador) (Nil, n) xs

elem n xs = elemAgg (fst (elemAux n (reverse xs)))

--------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------

-- Problema 4:
-- Utilizar la funcion "fold" para definir
-- la funcion "update". Esta funcion debe
-- tomar un numero "i", un valor "v" y una
-- lista. Debe retornar una lista donde
-- el elemento en la posicion "i" es
-- reemplazado por "v". POr ejemplo:
-- update 0 42 [1,2,3] == [42,2,3]
-- update 2 42 [1,2,3] == [1,2,42]

update :: Eq t => t -> t -> [t] -> [t]
update _ _ [] = []
update' i v xs = 
    map (\h -> 
        if h == i 
            then v  else h) xs

--------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------

-- Problema 5:
-- Utilizar la funcion "fold" para definir
-- la funcion "map". En otras palabras,
-- provea una definicion alterna de la
-- funcion "map" que este definida en
-- terminos de la funcion fold.

map _ Nil = Nil 
map a (Cons x xs) = Cons (a x) (map a xs) 

map' a estado x xs = (Cons a x, estado a xs)

fold' map' estado x xs = map' (fold map' estado xs) x
--------------------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------------------------------------------------

-- Ejercicios de repaso:
-- A continuacion se proveen ejercicios
-- opcionales que puede elaborar para
-- estudiar para su examen parcial:

-- (1)
-- Definir las funciones "maximo comun divisor"
-- y "minimo comun multiplo" utilizando Haskell.

mcdAux n m i d
  | n == i || m == i = d
  | mod n (i + 1) == 0 && mod m (i + 1) == 0 = mcdAux n m (i + 1) (i + 1)
  | otherwise = mcdAux n m (i + 1) d

mcd :: Int -> Int -> Int
mcd n m = mcdAux n m 1 1

mcm n m = undefined

----------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------

-- (2)
-- Definir la funcion "raiz cuadrada" utilizando
-- Haskell. Esta version de raiz cuadrada no
-- debe ser exacta, solo debe retornar el numero
-- entero mas cercano (pero menor) a la raiz
-- cuadrada del numero. No utilize las funciones
-- incluidas en Haskell como "floor" y "sqrt"
-- en su definicion. Ejemplo:\
--
-- raiz 9 == 3
-- raiz 10 == 3
-- raiz 8 == 2
-- raiz 2 == 1

raizAux n i =
    if (i + 1) * (i + 1) > n
    then i
    else raizAux n (i + 1)

----------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------

-- raiz 4
-- = raizAux 4 1
-- | n = 4, i = 1
-- = if (i + 1) * (i + 1) > n then i else raizAux n (i + 1) [n/4][i/1]
-- = if (1 + 1) * (1 + 1) > 4 then 1 else raizAux 4 (1 + 1)
-- = if 2 * 2 > 4 then 1 else raizAux 4 2
-- = if 4 > 4 then 1 else raizAux 4 2
-- = raizAux 4 2
-- | n = 4, i = 2
-- = if (i + 1) * (i + 1) > n then i else raizAux n (i + 1) [n/4][i/2]
-- = if (2 + 1) * (2 + 1) > 4 then 2 else raizAux 4 (2 + 1)
-- = if 3 * 3 > 4 then 2 else raizAux 4 (2 + 1)
-- = if 9 > 4 then 2 else raizAux 4 3
-- = 2
raiz :: Int -> Int
raiz n = raizAux n 1

----------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------

-- (3)
-- Definir la funcion "convertirALista" utilizando
-- Haskell. Esta funcion toma un numero entero
-- y produce una lista donde cada elemento de la misma
-- es uno de los digitos del numero. Por ejemplo:
--
-- convertirALista 42 == Cons 4 (Cons 2 Nil)
-- convertirALista 712 == Cons 7 (Cons 1 (Cons 2 Nil))

reverse Nil = Nil
reverse (Cons x xs) = pushBack x (reverse xs)


convertirAListaAux 0 = Nil
convertirAListaAux n = Cons (mod n 10) (convertirAListaAux (div n 10))

convertirALista 0 = Cons 0 Nil
convertirALista n = reverse (convertirAListaAux n)

----------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------


-- (4)
-- Definir la funcion "convertirANumero" utilizando
-- Haskell. Esta funcion toma una lista de numeros
-- como parametro que representan los digitos de
-- un numero. Debe producir como resultado el
-- numero correspondiente. Intente definir
-- esta funcion utilizando "fold". Por ejemplo:

-- convertirANumero (Cons 4 (Cons 2 Nil)) == 42
-- convertirANumero (Cons 7 (Cons 1 (Cons 2 Nil))) == 712

convertirANumeroAux p Nil = 0
convertirANumeroAux p (Cons x xs) = x * p + convertirANumeroAux (p * 10) xs

-- convertirANumero (Cons 7 (Cons 1 (Cons 2 Nil)))
-- = convertirANumeroAux 1 (reverse xs) [xs/Cons 7 (Cons 1 (Cons 2 Nil))]
-- = convertirANumeroAux 1 (reverse (Cons 7 (Cons 1 (Cons 2 Nil))))
-- = convertirANumeroAux 1 (Cons 2 (Cons 1 (Cons 7 Nil)))
-- = x * p + convertirANumeroAux (p * 10) xs [x/2, xs/Cons 1 (Cons 7 Nil), p/1]
-- = 2 * 1 + convertirANumeroAux (1 * 10) (Cons 1 (Cons 7 Nil))
-- = 2 * 1 + convertirANumeroAux 10 (Cons 2 (Cons 7 Nil))
-- = 2 * 1 + x * p + convertirANumeroAux (p * 10) xs [x/1, xs/Cons 7 Nil, p/10]
-- = 2 * 1 + 1 * 10 + convertirANumeroAux (10 * 10) (Cons 7 Nil)
-- = 2 * 1 + 1 * 10 + convertirANumeroAux 100 (Cons 7 Nil)
-- = 2 * 1 + 1 * 10 + x * p + convertirANumeroAux (p * 10) xs [x/7, xs/Nil, p/100]
-- = 2 * 1 + 1 * 10 + 7 * 100 + convertirANumeroAux (100 * 10) Nil
-- = 2 * 1 + 1 * 10 + 7 * 100 + convertirANumeroAux 1000 Nil
-- = 2 * 1 + 1 * 10 + 7 * 100 + 0

convertirANumero xs = convertirANumeroAux 1 (reverse xs)

----------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------

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

esMayorQue5 x = x > 5

esPar n = mod n 2 == 0

filterAgg (cond, xs) x =
    if cond x
    then (cond, Cons x xs)
    else (cond, xs)

ifThenElse True t f = t
ifThenElse False t f = f

filter cond xs = snd (fold filterAgg (cond, Nil) xs)

filterAgg' cond estado x =
    ifThenElse (cond x) (Cons x estado) estado

filter' cond xs = fold (filterAgg' cond) Nil xs

filter'' cond Nil = Nil
filter'' cond (Cons x xs) = ifThenElse (cond x) (Cons x (filter'' cond xs)) (filter'' cond xs)

-------------------------------------------------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------------------------------------------------

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

-----------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------

--Examen Parcia 2 
-- PROBLEMA 2 
--Implemente en Haskell la funcion minimo comun multiplo 
-- de tal forma que dicha funcion acepta tres numeros diferentes
-- y retorna el minimo comun multiplo de esos tres numeros.
-- El minimo comun multiplo es el numero mas puequeño que 
--simultaneamente es un multiplo de los tres numeros que 
-- se dieron como parametro.Ejemplo:
-- mcm3 2 5 7 == 70
-- mcm3 3 10 15 == 30
-- Adicionalmente, elabore la reduccion de esta funcion para 
-- "mcm 2 3 4". Por brevedad, solo reduzca 3 recursiones.

mcmAux :: int -> int -> int -> int -> int
mcmAux n m z i d =
 if i < n || i < m ||i < z 
then d (i - 1)
 else if mod i n == 0 && mod i m == 0 && mod i z == 0
 then mcmAux n m z (i - 1) i
 else mcmAux n m z (i - 1) d (i - 1)

mcm n m z = mcmAux n m z (n *m *z) (n * m * z)

--------------------------------------------------------------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

-- PROBLEMA 4
-- Implemente en Haskell la funcion minimo comun multiplo 
--de tal forma que dicha funcion acepte una lista de numeros
-- y retorne el minimo comun multiplo de dicha lista. 
--El minimo comun multiplo es el numero mas puequeño que 
--simultaneamente es un multiplo de todos los numeros en 
--la lista que se dio como parametro. Ejemplo:
-- mcmL (Cons 2 (Cons 5 (Cons 7 Nil))) == 70
-- mcmL (Cons 3 (Cons 10 (Cons 15 Nil))) == 30

data | Cons integer lista | Nil deriving show a
Contar :: Lista -> Int 
Contar Nil=0
Contar (cons m nm)1 + contar nm

mcm: Lista -> Int 
mcmAux n m z i d
x 1 =1 | mn 1 = 1
then d 
else if mcmAux n nm x ( i +1) == 0 && mcm mn (i + 1) ==0
    then mcmAux n nm (i +1)(i+1)
    else mcmAux n nm (i i+d)d
-------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
-- PROBLEMA 6 
-- Dada la siguiente funcion "mcm" que acepta dos numeros 
-- y produce el minimo comun multiplo de los mismos:
-- mcmAux n m i d =
-- if i < n || i < m
-- then d
-- else if mod i n == 0 && mod i m == 0
-- then mcmAux n m (i - 1) i
-- else mcmAux n m (i - 1) d
-- mcm n m = mcmAux n m (n * m) (n * m)
-- Utilize esta funcion en conjunto con la funcion "fold" 
-- estudiada en clase para implementar nuevamente la 
-- funcion minimo comun multiplo para listas.

fold:: (n -> m -> n) -> n -> (m) -> n
foldl (m -> n -> m) m (m n)

mcmAux n m i d =
 if i < n || i < m
 then d
 else if mod i n == 0 && mod i m == 0
 then mcmAux n m (i - 1) i
 else mcmAux n m (i - 1) d

mcm n m = mcmAux n m (n * m) (n * m)

main = undefined
