{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Show, float,  (+), (-),(*), (/), (>), (||), (&&), fst, True, false)

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

pushAgregador estado x =  Contras x estado

pushBack x xs =
    plegar pushAgregador ( Contras x Nil ) xs

-- Problema 2:
-- Utilizar la funcion "fold" para definir
-- la funcion "take". La funcion "take" debe
-- tomar una lista y un numero "n" y extraer
-- de la lista los primeros "n" elementos
-- que aparecen en la lista.
-- Puede basarse en la funcion "drop" para
-- su implementacion.

tomar  ::  Int  -> [ a ] -> [ a ]
tomar n xs =  foldr paso ( Contras [a]) xs n
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
elemAgg ( Contras x Nulo ) = x

elemAgregador (resultado, n) x = 
    si n ==  0 
        entonces un
        más b

    dónde 
        a = ( Contras x Nulo , n -  1 )
        b = (resultado, n -  1 )

elemAux n xs = fold (elemAgregador) ( Nil , n) xs

elem n xs = elemAgg ( fst (elemAux n ( reversa xs)))

-- Problema 4:
-- Utilizar la funcion "fold" para definir
-- la funcion "update". Esta funcion debe
-- tomar un numero "i", un valor "v" y una
-- lista. Debe retornar una lista donde
-- el elemento en la posicion "i" es
-- reemplazado por "v". POr ejemplo:
-- update 0 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 42 (Cons 2 (Cons 3 Nil)))
-- update 2 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 1 (Cons 2 (Cons 42 Nil)))

actualizar []  []  Nil  =  Nil

actualizar iv ns = mapu ( \ h ->
    si h == i
    entonces v
    más h) ns

-- Problema 5:
-- Utilizar la funcion "fold" para definir
-- la funcion "map". En otras palabras,
-- provea una definicion alterna de la
-- funcion "map" que este definida en
-- terminos de la funcion fold.

map :: (a -> b) -> [a] -> [b]
map f [a, b] = [a, b]
map f (x:xs) = foldr (x xs -> (f x) : xs)

mapu []  Nil  =  Nil
mapa x ( Cons n ns) =  Cons (xn) ( mapa x ns)

mapa x estado n ns = ( Cons x ns, estado x ns)

-- Ejercicios de repaso:
-- A continuacion se proveen ejercicios
-- opcionales que puede elaborar para
-- estudiar para su examen parcial:

-- (1)
-- Definir las funciones "maximo comun divisor"
-- y "minimo comun multiplo" utilizando Haskell.

mcdAux n m i d =
if n == 1 || m ==1
then d 
else if mod n (1 + 1) ==0 && m(1 + 1) == 0
then mcdaux n m (1 + 1) (1 + 1)
else mcd aux n m (1 + 1)d

mcd :: int -> int -> int 
mcd n m = mcdaux n m 1 1



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
if (1 + 1) * (1 + 1) > n 
then `
else raizaux n (1 + 1)

-- (3)
-- Definir la funcion "convertirALista" utilizando
-- Haskell. Esta funcion toma un numero entero
-- y produce una lista donde cada elemento de la misma
-- es uno de los digitos del numero. Por ejemplo:
--
-- convertirALista 42 == Cons 4 (Cons 2 Nil)
-- convertirALista 712 == Cons 7 (Cons 1 (Cons 2 Nil))

pushback n Nil = cons n Nil 
reverse

reverse Nil = Nil

convertirALista 0 = cons 0 Nil 
convertir n = cons (mod n 10) (convertirALista (div n 10))

convertirALista 0 = cons 0 Nil
convertirALista n = reverse (convertirAListaAux n)

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
convertirANumeroAux (cons x xs) = x + p + convertirANumeroAux (p *10) n

convertirANumeroAux xs = convertirANumeroAux (reverse xs)

-- (5)
-- Utilize la funcion "fold" para definir la funcion
-- "filter". La funcion "filter" toma una funcion
-- como parametro que representa una condicion y
-- produce una nueva lista con solamente los elementos
-- de la lista original que cumplen esa condicion.
-- Por ejemplo:
--
-- esMayorQue5 x = x > 5
-- filter esMayorQue5 (Cons 4 (Cons 5 (Cons 6 (Cons 7 Nil)))) == Cons 6 (Cons 7 Nil)\

filterAgg xs = cons x xs  

filter cond xs = fold filterAgg Nil xs 

filterAgg' cond xs x=
    if cond
        the  



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
