{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Show, fst, undefined)

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

update :: Eq t => t -> t -> [t] -> [t]

 update _ _ [] = []

 update' i v  xs = 
     map (\h -> 
         if h == i 
             then v 
             else h) xs


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
raizAux n i = 
    if (i + 1) * (i + 1) > n
        then i
        else raizAux n (i + 1)
    
raiz :: Int -> Int
raiz n = raizAux n i 
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

convertirAListaAuxiliar 0 = Nil
convertirAListaAuxiliar n = Cons (mod n 10) (convertirAListaAuxiliar (div n 10 ))

convertirALista 0 = Nil
convertirALista n = reverse ( convertirAListaAuxiliar n )







-- (4)
-- Definir la funcion "convertirANumero" utilizando
-- Haskell. Esta funcion toma una lista de numeros
-- como parametro que representan los digitos de
-- un numero. Debe producir como resultado el
-- numero correspondiente. Intente definir
-- esta funcion utilizando "fold". Por ejemplo:

-- convertirANumero (Cons 4 (Cons 2 Nil)) == 42
-- convertirANumero (Cons 7 (Cons 1 (Cons 2 Nil))) == 712



convertirANumeroAux Nil = 0 
convertirANumeroAux (Cons x xs) = undefined


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


main = undefined