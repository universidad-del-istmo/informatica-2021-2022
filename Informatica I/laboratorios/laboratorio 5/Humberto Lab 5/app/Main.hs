{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Show, Float , Bool (..) , fst, floor, lcm, (+), (-), mod, (==), floor, (^),  (||), (*), (<), (>), (&&), (**), fst, snd,  undefined, div)

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

--repuesta
--pushBack 3 (Cons 1 (Cons 2 Nill))
-- | x = 3, xs = (Cons 1 (Cons 2 Nill))
-- "pushBack 3 (Cons 1 (Cons 2 Nil))"
-- = fold pushAgregador (Cons x Nil) xs [x/3, xs/(Cons 1 (Cons 2 Nil))]
-- = fold pushAgregador (Cons 3 Nil) (Cons 1 (Cons 2 Nil))
-- | fold:  agg (fold agg cero xs) x [agg/ pushAgregador, cero/Cons 3, x/Nil, xs /(Cons 1 (Cons 2 Nil)) ]
-- = pushAgregador (fold pushAgregador Cons 3 (Cons 1 (Cons 2 Nil))) Nil
-- | fold:  agg (fold agg cero xs) x  [agg/pushAgregador, cero/Cons 2, x /2, xs/Cons 3 (Cons 1 Nil ]
-- = pushAgregador (pushAgregador (fold pushAgregador  Cons 2 (Cons 3 (Cons 1 Nil)))2) Nil)
-- | fold: agg = pushAgregador, cero = Cons 2, x = 2,  xs = (Cons 3 (Cons 1 Nil))
-- = pushAgregador (pushAgregador Cons 2 (Cons 3 (Cons 1 Nil))) Nil 
-- | pushAgregador:  Cons x estado [estado/Cons 2, x / (Cons 3 (Cons 1)) , Nil
-- = Cons (Cons 3 (Cons 1) (Cons 2)) Nil
-- | pushAgregador:  Cons x estado [estado/Cons 3, x / (Cons 1 (Cons 2)) , Nil
-- = Cons (Cons 1 (Cons 2)) Cons 3) Nil 
-- | Cons x estado [Cons/ Cons 1, x/Cons 2 / estado / Cons 3 Nil]
-- Cons 1 (Cons 2 (Cons 3 Nil))




pushAgregador estado x = Cons x estado

pushBack x = fold pushAgregador (Cons x Nil)

-- Problema 2:
-- Utilizar la funcion "fold" para definir
-- la funcion "take". La funcion "take" debe
-- tomar una lista y un numero "n" y extraer
-- de la lista los primeros "n" elementos
-- que aparecen en la lista.
-- Puede basarse en la funcion "drop" para
-- su implementacion.


hReverse Nil = Nil

hReverse (Cons x xs) = pushBack x (hReverse xs)



takeAgregador (resultado, n) x =
    if  n > 0

         then
        (pushBack x resultado, n - 1)

        else
        (resultado, n - 1)



takeAux n = fold takeAgregador (Nil, n)

hTake n xs = fst (takeAux n (hReverse xs))




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

getElem (Cons x Nil) = x
elemAgregador (resultado, n) x =

    if n == 0

        then
        (Cons x Nil, n - 1)

        else
        (resultado, n - 1)


elemAux n xs = fold elemAgregador (Nil, n) xs

hElem n xs = getElem (fst (elemAux n (hReverse xs)))

-- Problema 4:
-- Utilizar la funcion "fold" para definir
-- la funcion "update". Esta funcion debe
-- tomar un numero "i", un valor "v" y una
-- lista. Debe retornar una lista donde
-- el elemento en la posicion "i" es
-- reemplazado por "v". POr ejemplo:
-- update 0 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 42 (Cons 2 (Cons 3 Nil)))
-- update 2 42 (Cons 1 (Cons 2 (Cons 3 Nil))) == (Cons 1 (Cons 2 (Cons 42 Nil)))

update _ _ Nil = Nil

hUpdate a c = map (\h ->
  if h == a
      then c
      else h)



-- Problema 5:
-- Utilizar la funcion "fold" para definir
-- la funcion "map". En otras palabras,
-- provea una definicion alterna de la
-- funcion "map" que este definida en
-- terminos de la funcion fold.

map _ Nil = Nil

map a (Cons x xs) = Cons (a x) (map a xs)


hMap a estado x xs = (Cons a x, estado a xs)

hFold hMap estado x xs = hMap (fold hMap estado xs) x








-- Ejercicios de repaso:
-- A continuacion se proveen ejercicios
-- opcionales que puede elaborar para
-- estudiar para su examen parcial:

-- (1)
-- Definir las funciones "maximo comun divisor"
-- y "minimo comun multiplo" utilizando Haskell.

-- mcdAux n m i d
 -- | n == i || m == i = d
 -- | mod n (i + 1)  == 0 && mod m (i + 1) == 0 = mcdAux n m (i + 1) (i + 1)
--  | otherwise = mcdAux n m (i + 1) d










--reducción mcm 2 3 4
-- mcmDos 2 3 5 (2* 3 * 4)(2 * 3* 4)
-- | mcmDos n m l q p =
-- | mcmDos n = 2, m = 3, l = 5  p = 12
-- | if q < n || q < m || q < l then p
-- = if q < 2 || q < 3 || q < 1 then 12
-- | else if mod q n == 0 && mod q m == 0 && mod q l == 0 [n/2, m/3, l/5]
-- = else if mod q 2 == 0 && mod q 3 == 0 && mod q 5 == 0






--mcmL (Cons x xs) = mcmAux (Cons x xs) Cons (x * xs)(x * xs)



--fold' agg cero n m  =
 --   agg (fold' agg cero n) m

--mcmAux n m i d =

-- if i < n || i < m

-- then d

 --else if mod i n == 0 && mod i m == 0

 --then mcmAux n m (i - 1) i

 --else mcmAux n m (i - 1) d


--mcmF n m = 
 --   fold mcmAux n m  







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

--raizAux n i =
 --     if (i + 1) * (i + 1) > n
  --    then i
  --    else raizAux n (i + 1)

-- raiz 4
-- = raizAux 4 1 
--  | n = 4, i = 1
-- = if (i + 1) * (i + 1) > n  then i   else raizAux n (i + 1) [n/4, i/1]
-- = if (1 + 1) * (1 + 1) > 4  then 1   else raizAux 4 (1 + 1) 
-- = if (2) * (2) > 4  then 1   else raizAux 4 (2) 
-- = raizAux 4 (2)
-- | n = 4, i= 2
-- = if (i + 1) * (i + 1) > n  then i  else raizAux n (i + 1) 
-- = if (2 + 1) * (2 + 1) > 4  then 2  else raizAux 4 (2 + 1) [n/4, i/2] 
-- = if (3) * (3) > 4  then 2  else raizAux 4 (3)
-- = if 9  > 4  then 2  else raizAux 4 (3)
-- then 2  



--raiz :: Int -> Int
--raiz n = raizAux n 1

--raiz´ n = sqrt (floor n)





-- (3)
-- Definir la funcion "convertirALista" utilizando
-- Haskell. Esta funcion toma un numero entero
-- y produce una lista donde cada elemento de la misma
-- es uno de los digitos del numero. Por ejemplo:
--
-- convertirALista 42 == Cons 4 (Cons 2 Nil)
-- convertirALista 712 == Cons 7 (Cons 1 (Cons 2 Nil))

--ushBack n Nil = Cons n Nil 
--pushBack n (Cons x xs) = Cons x (pushback n xs)


--reverse Nil = Nil
--reverse (Cons x xs) = pushBack x (reverse xs)


--convertirAListaAux 0 =  Nil
--convertirAListaAux n = Cons (mod n 10)  (convertirAListaAux (div n 10))


--convertirALista 0 = Cons 0 Nil
--convertirALista n = reverse (convertirAListaAux n)





-- (4)
-- Definir la funcion "convertirANumero" utilizando
-- Haskell. Esta funcion toma una lista de numeros
-- como parametro que representan los digitos de
-- un numero. Debe producir como resultado el
-- numero correspondiente. Intente definir
-- esta funcion utilizando "fold". Por ejemplo:

-- convertirANumero (Cons 4 (Cons 2 Nil)) == 42
-- convertirANumero (Cons 7 (Cons 1 (Cons 2 Nil))) == 712


--convertirANumeroAux p Nil = 0
--convertirANumeroAux p (Cons x xs) = x * p + convertirANumeroAux (p * 10) xs

main = undefined

