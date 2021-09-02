{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (
    Bool(..),
    Int,
    Float,
    Show,
    (-),
    (+),
    (==,),
    (*),
    mod,
    undefined
    )
import GHC.Base (undefined)

data Lista = Cons Int Lista | Nil deriving Show
-- Se utiliza la palabra reservada (data) 
-- (Cons) es una palabra arbitraria y se utiliza para representar algo que tengo conectado con algo más 
-- (Int) representa el tipo de dato/objeto con el que se trabajará 
-- (List) a es a donde estará conectado el dato/objeto
-- (Nil deriving Show) se utiliza para representar una lista vacía 

--------------------------------------------------------------------------------

-- Problema #1:
-- A continuacion se presenta la definicion de la funcion "contar"
-- para una Lista. Escriba manualmente todos los
-- pasos que Haskell debe hacer pare reducir esta
-- funcion cuando es llamada asi:
-- contar (Cons 1 (Cons 2 (Cons 3 Nil)))
contar :: Lista -> Int
contar Nil = 0
contar (Cons n ns) = 1 + contar ns

-- DEMOSTRACIÓN PROBLEMA #1
-- contar (Cons 1 (Cons 2 (Cons 3 Nil)))
--  | n = 1, ns = (Cons 2 (Cons 3 Nil)) -- Identificar las variables en la función 
-- 1 + Contar (Cons 2 (Cons 3 Nil))) -- Sustituir en 1 + contar ns
--  | n = 2, ns = (Cons 3 Nil) -- Identificar las variables en la función
-- 1 + 2 + Contar (Cons 3 Nil) -- Sustituir en 1 + contar ns
--  | n = 3, ns = Nil -- Identificar las variables en la función
-- 1 + 1 + 1 + Contar (Nil) -- Sustituir en 1 + contar ns
--  |n = Nil -- Identificar las variables en la función
-- 1 + 1 +1 + 0 -- Nil es 0, dado que se indica en la función contar Nil = 0
-- 3 -- Resultado 

--------------------------------------------------------------------------------

-- Problema #2
-- Defina un nuevo tipo llamado "ListaTransformaciones". Este
-- tipo debe ser igual al tipo Lista definido arriba
-- pero en vez de tener numeros (Int) como elementos, debe
-- tener matrices de 2x2 como las que se utilizaron en el
-- laboratorio anterior.
data ListaTransformaciones = Cons ((Float, Float), (Float, Float)) ListaTransformaciones | Nil deriving Show

-- DEMOSTRACIÓN PROBLEMA #2
-- En el problema 3 del Laboratorio 3 se utilizó la función transformacionLineal 
-- ((m11, m12), (m21, m22)) para definir matrices de 2x2 
-- Las matrices (de tipo Float) se sustituyen en la ecuación 
-- data Lista = Cons Int Lista | Nil deriving Show
-- Lista = ListaTransformaciones 
-- Int es sustituido con las matrices 
-- Un ejemplo puede ser: Cons ((1.5, 2.5) (Cons 3.5, 4.5))

--------------------------------------------------------------------------------

-- Problema #3
-- Defina una funcion llamada "aplicarTransformaciones". Esta
-- funcion debe aceptar una "ListaTransformaciones" y un vector
-- de 2 elementos (matriz de 2X1). Esta funcion debe aplicar
-- todas las tranformaciones en la lista una despues de la otra
-- actualizando el resultado cada vez que se aplica una
-- transformacion.
aplicarTransformaciones :: ListaTransformaciones -> (Float, Float) -> (Float, Float)
aplicarTransformaciones Nil v = v
aplicarTransformaciones (Cons n ns) v = aplicarTransformaciones ns (transformacionLineal n v)

-- DEMOSTRACIÓN PROBLEMA #3
-- aplicarTransformaciones Nil v = v, indica que una lista vacia junto con un vector, 
-- da como resultado un vector, el cual es una matriz de dos elementos 
-- La función aplicarTransformaciones toma como parámetro n, el cual es un número
-- y también ns que representa un conjunto de números 
-- aplicarTransformaciones ns (transformacionLineal n v) permite que el resultado 
-- se actualice al aplicar una transformación 

--------------------------------------------------------------------------------

-- Problema #4
-- Defina una funcion llamada "sonIguales". Esta funcion
-- toma dos valores de tipo "Lista". Debe retornar "True" si
-- ambas listas tienen los mismos valores en la misma posicion.
sonIguales :: Lista -> Lista -> Bool
sonIguales Nil Nil = True 
sonIguales (Cons a as) (Cons b bs) = 
    (
        a == b) &&
        sonIguales a as 

-- DEMOSTRACIÓN PROBLEMA #4
-- sonIguales Nil Nil = True, se utiliza para que exista una comparación entre dos listas
-- Se comparan los valores de ambas listas utilizando if, en este caso n y x 
-- Si ambos valores son iguales, la función retorna True, de lo contrario retorna False 
--------------------------------------------------------------------------------

-- Problema #5
-- La aritmetica modular es aquella aritmetica que funciona como la
-- aritmetica tradicional, pero se escoge un numero "m" llamado el
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
sumarLista :: Lista -> Int 
sumarLista Nil = 0
sumarLista (Cons m ms) = m + sumarLista ms 
sonEquivalentes :: Int -> Lista -> Lista -> Bool
sonEquivalentes a x1 x2 == mod (sumarLista x1) a == mod (sumarLista x2) a 

-- DEMOSTRACIÓN PROBLEMA #5
-- sumarLista Nil = 0 indica que una lista vacía tiene como resultado 0. 
-- sumarLista (Cons n ns) se refiere a la suma de un numero más la función y ns 
-- Si ambos valores son iguales, la función retorna True, de lo contrario retorna False 
--------------------------------------------------------------------------------


main = undefined
