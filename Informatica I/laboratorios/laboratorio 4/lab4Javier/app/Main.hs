{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (
    Bool(..),
    Int,
    Float,
    Show,
    (-),
    (+),(|), 
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
contar  ::  Lista  ->  Int
contar Nil  =  0
contar ( Cons n ns) =  1  + contar ns
- contar (Contras 1 (Contras 2 (Contras 3 Nil)))
-  | n = 1 ns = (Contras 2 (Contras 3 Nill))
- 1 + contar (Contras 2 (Contras 3 Nill))
-  | n = 2 ns = (Contras 3)
- 1 + 1 + contar (Contras 3)
-  | n = 3 ns = Nulo
- 1 +1 +1 + contar (cero)
- 1 + 1 + 1 + 0
- 3
-- este codigo fue trabajo con unos compañeros por si se encuentran similitudes 

-- Problema #2
-- Defina un nuevo tipo llamado "ListaTransformaciones". Este
-- tipo debe ser igual al tipo Lista definido arriba
-- pero en vez de tener numeros (Int) como elementos, debe
-- tener matrices de 2x2 como las que se utilizaron en el
-- laboratorio anterior.
data  ListaTransformaciones  =  Matriz ( Float , Float ) ( Float , Float ) ListaTransformaciones | Show de derivación nula

-- Problema #3
-- Defina una funcion llamada "aplicarTransformaciones". Esta
-- funcion debe aceptar una "ListaTransformaciones" y un vector
-- de 2 elementos (matriz de 2X1). Esta funcion debe aplicar
-- todas las tranformaciones en la lista una despues de la otra
-- actualizando el resultado cada vez que se aplica una
-- transformacion.
aplicarTransformaciones  ::  ListaTransformaciones  -> ( Float , Float ) -> ( Float , Float )
transformacionLineal  :: (( Float , Float ), ( Float , Float )) -> ( Float , Float ) -> ( Float , Float )

transformacionLineal ((m11, m12), (m21, m22)) (x, y) = 
    (((m11 * x) + (m12 * y)), ((m21 * x) + (m22 * y)))

aplicarTransformaciones Nul (x, y) = (x, y)
aplicarTransformaciones ( Matriz (m11, m12) (m21, m22) s) (x, y) = aplicarTransformaciones s (transformacionLineal ((m11, m12), (m21, m22)) (x, y))

-- Defina una funcion llamada "sonIguales". Esta funcion
-- toma dos valores de tipo "Lista". Debe retornar "True" si
-- ambas listas tienen los mismos valores en la misma posicion.
sonIguales :: Lista -> Lista -> Bool

sonIguales O O = Succ O
sonIguales O m = O
sonIguales n O = O
sonIguales (Succ a) (Succ b) = sonIguales a b

sonIguales Nil  Nil  =  true 
sonIguales ( Cons x xs) ( Cons y ys) = x == y && sonIguales (xs) (ys) 

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
sonEquivalentes  ::  Int  ->  Lista  ->  Lista  ->  Bool
sonEquivalentes 0 ( Cons a as) ( Cons b bs) =  True  - No se puede dividir entre 0, en teoria es el mismo resultado
sonEquivalentes m ( Cons a as) ( Cons b bs) =  mod (contar2 ( Cons a as)) m ==  mod (contar3 ( Cons b bs)) m

main = undefined