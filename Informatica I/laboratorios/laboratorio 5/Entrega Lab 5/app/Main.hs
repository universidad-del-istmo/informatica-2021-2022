{-# LANGUAGE NoImplicitPrelude #-}

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

obtenerelement (Cons x Nil) = x

elemenAgregador (resultado, n) x = 
  if n == 0 then 
    (Cons x Nil, n -1)
    else 
      (resultado, n-1)

elemenAux n xs = fold (elemenAgregador) (Nil, n) xs 

elem n xs = obtenerelement (fst (elemenAux n (reverse xs)))


-- Problema 4:
-- Utilizar la funcion "fold" para definir
-- la funcion "update". Esta funcion debe
-- tomar un numero "i", un valor "v" y una
-- lista. Debe retornar una lista donde
-- el elemento en la posicion "i" es
-- reemplazado por "v". POr ejemplo:
-- update 0 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 42 (Cons 2 (Cons 3 Nil)))
-- update 2 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 1 (Cons 2 (Cons 42 Nil)))

--updateAgregador (resultado, i) v xs = 
  --if i == 0 then 
    --(Cons i Nil) 
    --else 
     -- (resultado, i)
--updateAux i v xs = fold (updateAgregador i, v) xs 

--update i v (Cons x xs) = fold updateAux v i (Cons x xs)

update _ _ Nil = Nil

update1 i v xs = map (\h -> 
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

mapf f estado x xs = (Cons f x, estado f xs)

foldm mapf estado x xs = mapf (fold mapf estado xs) x 




--map1 _ Nil = Nil
--map1= foldmap map f (Cons x xs)
--map1 estado (Cons x xs) = Cons (estado x) (map1 estado xs)
--foldmap map1 f (Cons x xs)= map1 (fold map1 )

-- Ejercicios de repaso:
-- A continuacion se proveen ejercicios
-- opcionales que puede elaborar para
-- estudiar para su examen parcial:

-- (1)
-- Definir las funciones "maximo comun divisor"
-- y "minimo comun multiplo" utilizando Haskell.


mcdAux n m i d = 
 if  n == i || m == i
    then d
    else if  mod n (i + 1) == 0 && mod m (i+1) == 0 
    then mcdAux n m (i +1) (i+1)
    else mcdAux n m (i +1) d

mcd n m = mcdAux n m 1 1 


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
  if (i+1) * (i+1) > 0 
    then i 
    else raizAux n (i + 1) 

raiz n = raizAux n 1



-- (3)
-- Definir la funcion "convertirALista" utilizando
-- Haskell. Esta funcion toma un numero entero
-- y produce una lista donde cada elemento de la misma
-- es uno de los digitos del numero. Por ejemplo:
--
-- convertirALista 42 == Cons 4 (Cons 2 Nil)
-- convertirALista 712 == Cons 7 (Cons 1 (Cons 2 Nil))

convertirAListaAux 0 = Nil
convertirAListaAux x = Cons (mod x 10) (convertirALista (div x 10))

convertirALista 0 = Cons 0 Nil
convertirALista n = reverse (convertirALista n)




-- (4)
-- Definir la funcion "convertirANumero" utilizando
-- Haskell. Esta funcion toma una lista de numeros
-- como parametro que representan los digitos de
-- un numero. Debe producir como resultado el
-- numero correspondiente. Intente definir
-- esta funcion utilizando "fold". Por ejemplo:

-- convertirANumero (Cons 4 (Cons 2 Nil)) == 42
-- convertirANumero (Cons 7 (Cons 1 (Cons 2 Nil))) == 712

--convertirANumero Cons x xs= x convertirANumero xs 

convertirANumeroAux p Nil = 0 
convertirAux p (Cons x xs) = x * p + convertirANumeroAux (p *10) xs 

convertirANumero xs =convertirANumeroAux 1 (reverse xs)



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

filterAgregador (cond, xs) x =
  if cond x 
    then (cond, Cons x xs)
    else (cond, xs)

filterS cond xs = snd (fold filterAgregador(cond, Nil) xs)

filterAux cond xs x = 
  if cond x 
    then Cons x xs
    else xs

filter cond xs = fold (filterAux cond) Nil xs

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


existe n Nil = False 
existe n (Cons x xs) = n == x || existe n xs


existenValores n Nil = False
existenValores n (Cons x Nil) = False 
existenValores n (Cons x xs) = existe (n - x) xs || existenValores n xs 



main = undefined