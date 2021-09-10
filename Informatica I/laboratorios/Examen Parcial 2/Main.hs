{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude(
    Bool,
    Float, (-), (^), (+), (<=), (*), (<), (&&), (mod), 
    undefined
    )


--Problema 1 Implemente la funcion "maximo comun divisor" de tal forma que dicha funcion acepta tres numeros diferentes y retorna el maximo comun divisor de esos tres numeros.
mcd :: Integer -> Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (mcd a b)
mcd b c = mcd c (mcd b c)

main = do

t1 <- getLine 

t2 <- getLine 

t3 <- getline

let x = (read t1 :: Integer)
let y = (read t2 :: Integer)
let z = (read t3 :: Integer)
print(mcd x y z)



-- Problema 2 Implemente en Haskell la funcion minimo comun multiplo de tal forma que dicha funcion acepta tres numeros diferentes y retorna el minimo comun multiplo de esos tres numeros. 
--El minimo comun multiplo es el numero mas puequeño que simultaneamente es un multiplo de los tres numeros que se dieron como parametro.

mcmAux :: int -> int -> int -> int -> int
mcmAux n m z i d =
 if i < n || i < m ||i < z |
 then d (i - 1)
 else if mod i n == 0 && mod i m == 0 && mod i z == 0
 then mcmAux n m z (i - 1) i
 else mcmAux n m z (i - 1) d (i - 1)

mcm n m z = mcmAux n m z (n *m *z) (n * m * z)


--Problema 3 Implemente en Haskell la funcion maximo comun divisor de tal forma que dicha funcion acepte una lista de numeros y retorne el maximo comun divisor de los numeros en dicha lista.

take' :: Int -> [a] -> [a]
take' 0 _          = []
take' (n+1) []     = []
take' (n+1) (x:xs) = x : take' n xs



mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (a `mod` b)



-- Problema 4 Implemente en Haskell la funcion minimo comun multiplo de tal forma que dicha funcion acepte una lista de numeros y retorne el minimo comun multiplo de dicha lista.
--El minimo comun multiplo es el numero mas puequeño que simultaneamente es un multiplo de todos los numeros en la lista que se dio como parametro.

data lista | Cons integer lista | Nil deriving show a
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





--Problema 5 Utilize esta funcion en conjunto con la funcion "fold" estudiada en clase para implementar nuevamente la funcion maximo comun divisor para listas.
foldl :: (a -> b -> a) -> a -> [b] -> a

foldl f ys xs = aux ys xs

  where aux ys []     = ys

        aux ys (x:xs) = aux (f ys x) xs

        

mcdAux n m i d =

 if n == i || m == i
 then d
 else if mod n (i + 1) == 0 && mod m (i + 1) == 0
 then mcdAux n m (i + 1) (i + 1)
 else mcdAux n m (i + 1) d


mcd :: Int -> Int -> Int

mcd n m = mcdAux n m 1 1



--Problema 6 Dada la siguiente funcion "mcm" que acepta dos numeros y produce el minimo comun multiplo de los mismos: 

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
