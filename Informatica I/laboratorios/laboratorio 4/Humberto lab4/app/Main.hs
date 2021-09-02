{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (
    Bool(..),
    Int,
    Float,
    Show,
    (-),
    (+),
    (*),
    (&&),
    (==),
    mod,
    undefined
    )
import GHC.Base (undefined)

data Lista = Cons Int Lista | Nil deriving Show

-- Problema #1:
-- A continuacion se presenta la definicion de la funcion "contar"
-- para una Lista. Escriba manualmente todos los
-- pasos que Haskell debe hacer pare reducir esta
-- funcion cuando es llamada asi:
-- contar (Cons 1 (Cons 2 (Cons 3 Nil)))
contar :: Lista -> Int
contar Nil = 0
contar (Cons n ns) = 1 + contar ns

--repuesta 

-- contar (Cons 1 (Cons 2 (Cons 3 Nil)))
-- | n = 1, ns = (Cons 2 (Cons 3 Nil))
-- 1 + Contar (Cons 2 (Cons 3 Nil)))
-- | n = 2, ns = (Cons 3 Nil)  
-- 1 + 1 + Contar (Cons 3 Nil)
-- | n = 3, ns = Nil 
-- 1 + 1 + 1 + Contar (Nil)
-- n = Nil
-- 1 + 1 +1 + 0
-- 3





-- Problema #2
-- Defina un nuevo tipo llamado "ListaTransformaciones". Este
-- tipo debe ser igual al tipo Lista definido arriba
-- pero en vez de tener numeros (Int) como elementos, debe
-- tener matrices de 2x2 como las que se utilizaron en el
-- laboratorio anterior.


data ListaTransformaciones =
     Consi ((Float, Float), (Float, Float)) ListaTransformaciones | Nill deriving Show


--data ListaTransformaciones = PorSerDefinidio deriving Show






-- Problema #3
-- Defina una funcion llamada "aplicarTransformaciones". Esta
-- funcion debe aceptar una "ListaTransformaciones" y un vector
-- de 2 elementos (matriz de 2X1). Esta funcion debe aplicar
-- todas las tranformaciones en la lista una despues de la otra
-- actualizando el resultado cada vez que se aplica una
-- transformacion.

--colocamos tranformación lineall por fines didacticos

transformacionLineal :: ((Float, Float), (Float, Float)) -> (Float, Float) -> (Float, Float)
transformacionLineal ((m11, m12), (m21, m22)) (x, y) =  (matriz1, matriz2)

--(m11  * x + m12 * y, m21 * x + m22 * y )

    where
        matriz1 = m11 * x + m12 * y
        matriz2 = m21 * x + m22 * y


-- ahora completamos la definición de aplicarTransformaciones
-- cabe recordar que
-- vector = v 

aplicarTransformaciones :: ListaTransformaciones -> (Float, Float) -> (Float, Float)

aplicarTransformaciones Nill v =  v
aplicarTransformaciones (Consi n ns) v = aplicarTransformaciones ns (transformacionLineal n v)






-- Problema #4
-- Defina una funcion llamada "sonIguales". Esta funcion
-- toma dos valores de tipo "Lista". Debe retornar "True" si
-- ambas listas tienen los mismos valores en la misma posicion.
sonIguales :: Lista -> Lista -> Bool
sonIguales (Cons n ns)  (Cons a as) =

       listaN == listaS
    && listaS == listaN && a == n


    --contar (Cons n ns) == contar (Cons a as) 
   -- && contar (Cons a as) == contar (Cons n ns) 


    where
        listaN = contar (Cons n ns)
        listaS = contar (Cons a as)





-- Problema #5
-- La aritmetica modular es aquella aritmetica que funciona como la
-- aritmetica tradicional, pero se escogje un numero "m" llamado el
-- modulo. Todo valor y resultado de una operacion se le debe aplicar
-- el modulo (residuo) de la division para obtener el resultado final.
-- Por ejemplo:
-- Si m = 5
-- entonces:
-- 8 + 7 (mod 5) = 15 (mod 5) = 0
-- ya que "mod 15 5" es 0
-- En la aritmetica modular, varios numeros son equivalentes. Por
-- ejemplo, cuando "m = 5" como el ejemplo anterior, "7 = 2" ya que:
-- 8 + 7 (mod 5) = 15 (mod 5) = 0
-- 8 + 2 (mod 5) = 10 (mod 5) = 0
-- ya que "mod 15 5 = 0" y "mod 10 5 = 0"
-- Su tarea es definir una funcion llamada "sonEquivalentes". Esta
-- funcion acepta un valor "m" que representa un modulo y dos
-- Listas. Esta funcion debe retornar True si ambas listas tienen
-- los mismos valores en modulo "m" o False de lo contrario.
-- Por ejemplo:
-- sonEquivalentes 5 (Cons 1 (Cons 2 Nil)) (Cons 6 (Cons 7 Nil)) == True
-- sonEquivalentes 3 (Cons 1 (Cons 2 Nil)) (Cons 6 (Cons 7 Nil)) == False 




sonEquivalentes :: Int -> Lista -> Lista -> Bool
--primero se define la suma para  las listas y ya completamos la definición

--sumLista Nil = 0
--sumLista (Cons n ns) = n + sumLista ns

--sonEquivalentes  m (Cons n ns) (Cons a as) = mod (sumLista lista1) m  == mod (sumLista lista2) m
sonEquivalentes  m (Cons n ns) (Cons a as) = mod n m == mod a m  
 -- where
    --lista1 = Cons n ns 
   -- lista2 = Cons a as 





main = undefined



