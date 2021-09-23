{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Bool(..), Int, Float, Show, (-), (<), div, (==), (/), (+), mod, (^), (*), (&&), (||), undefined)
import GHC.Base (undefined)

data Lista = Nil | Cons Int Lista deriving Show 


--Problema #2
--Implemente en Haskell la funcion minimo comun multiplo de tal forma que dicha funcion acepta tres 
--numeros diferentes y retorna el minimo comun multiplo de esos tres numeros. El minimo comun multiplo es 
--el numero mas puequeño que simultaneamente es un multiplo de los tres numeros que se dieron como parametro.



--Ejemplo:
--mcm3 2 5 7 == 70
--mcm3 3 10 15 == 30

--Adicionalmente, elabore la reduccion de esta funcion para "mcm 2 3 4". Por brevedad, solo reduzca 3 recursiones.

mcmAux n m s i d =
    if i<n || i<m || i<s
        then d 
     else if mod i n == 0 && mod i m == 0 && mod i s == 0
        then mcmAux n m s (i-1)i
        else mcmAux n m s (i-1)d

mcm3 n m s = mcmAux n m s (n*m*s)(n*m*s) 

-- reduccion 
--mcm3 2 3 4 
-- n = 2, m = 3, s = 4 
-- = mcmAux 2 3 4 (24)(24)
-- n = 2, m = 3, s = 4, i = 24, d=24
-- = if i<n || i<m || i<s / then d (esto no se cumple)
-- else if mod i n == 0 && mod i m == 0 && mod i s == 0 (se cumple)
-- = mcmAux 2 3 4 23 24 (este ciclo continuara hasta que "i" sea menor que alguno de los 3 datos)
-- = n = 2, m = 3, s = 4, i = 23, d=24
-- if i<n || i<m || i<s (este no cumple)
-- else if mod i n == 0 && mod i m == 0 && mod i s == 0 (este tampoco cumple)
-- else mcmAux 2 3 4 (22) 24
-- n = 2, m = 3, s = 4, i = 22, d=24
-- else mcmAux n m s (21)24
-- n = 2, m = 3, s = 4, i = 21, d=24 no cumplira hasta qye i = 12
-- n = 2, m = 3, s = 4, i = 12, d=24
-- if i<n || i<m || i<s no cumple 
-- else if mod i n == 0 && mod i m == 0 && mod i s == 0 si cumple 
-- = mcmAux 2 3 4 (11)12
-- n = 2, m = 3, s = 4, i = 11, d=12
-- para simplificar, la unica regla que seguira en ciclo es: 
-- else mcmAux n m s (i-1)d
-- = mcmAux 2 3 4 (10)12 aca la unica regla que cumplira es if i<n || i<m || i<s hasta que i = 3
-- = mcmAux 2 3 4 (3)12 
-- = n = 2, m = 3, s = 4, i = 3, d=12
-- if i<n || i<m || i<s si cumple 
-- then 12
-- el mcm3 seria 12 



-- problema #4
--Implemente en Haskell la funcion minimo comun multiplo de tal 
--forma que dicha funcion acepte una lista de numeros y retorne 
--el minimo comun multiplo de dicha lista. 
--El minimo comun multiplo es el numero mas puequeño que 
--simultaneamente es un multiplo de todos los numeros en la lista 
--que se dio como parametro.

--Ejemplo:

--mcmL (Cons 2 (Cons 5 (Cons 7 Nil))) == 70
--mcmL (Cons 3 (Cons 10 (Cons 15 Nil))) == 30

mcm' (Cons x xs) i d = 
    if compara (Cons x xs) i 
        then d
        else if comparamod (Cons x xs) i 
            then mcm' (Cons x xs) (i-1)i
            else mcm' (Cons x xs) (i-1)d

multiplicatoria (Cons x xs) = x * (multiplicatoria xs)

mcmL Nil = 1
mcmL(Cons x xs) = mcm' (Cons x xs) (x * (mcmL xs)) (x*(mcmL xs))

compara Nil i = False 
compara (Cons x xs) i = i < x || compara xs i 

comparamod Nil i = True 
comparamod (Cons x xs) i = mod i x == 0 && comparamod xs i



--Problema # 6
--Dada la siguiente funcion "mcm" que acepta dos numeros y produce el 
--minimo comun multiplo de los mismos:

mcmAux2 n m i d =
    if i < n || i < m
        then d
        else if mod i n == 0 && mod i m == 0
            then mcmAux2 n m (i - 1) i
            else mcmAux2 n m (i - 1) d

mcm n m = mcmAux2 n m (n * m) (n * m)

--Utilize esta funcion en conjunto con la funcion "fold" estudiada en clase para 
--implementar nuevamente la funcion minimo comun multiplo para listas.


fold :: (estado -> Int -> estado) -> estado -> Lista -> estado

fold cons nil Nil = nil
fold cons nil (Cons x xs) = cons (fold cons nil xs) x

mcmf (Cons x xs) = mcm x (fold mcm 1 xs)


main = undefined
