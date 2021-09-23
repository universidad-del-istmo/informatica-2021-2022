{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (undefined, Float, Int, abs, pi, sqrt, (<), sin, cos, pi, exp, log, asin, acos, atan, sinh, cosh, asinh, acosh, atanh, (<=), (/), (+), (^), (*), (-), div, mod, (==), (/=), (>), (>=), even, (&&), (||), Bool (False, True), Show)

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
-- Respuesta:
-- Contar (Cons 1 (Cons 2 (Cons 3 Nil)))
-- |n = 1, ns = Cons2 (Cons 3 Nil)
-- = 1 + Contar (Cons 2 (Cons 3 Nil))
-- |n = 2, ns = (Cons 3 Nil)
-- = 1 + 1 + Contar (Cons 3 Nil)
-- |n = 3, ns = Nil
-- = 1 + 1 + 1 Contar Nil
-- |Contar Nil = 0|
-- = 1 + 1 + 1 + 0
-- = 3

-- Problema #2
-- Defina un nuevo tipo llamado "ListaTransformaciones". Este
-- tipo debe ser igual al tipo Lista definido arriba
-- pero en vez de tener numeros (Int) como elementos, debe
-- tener matrices de 2x2 como las que se utilizaron en el
-- laboratorio anterior.
data ListaTransformaciones = ConsMatrix ((Float, Float), (Float, Float)) ListaTransformaciones | NilMatrix deriving Show

-- Problema #3
-- Defina una funcion llamada "aplicarTransformaciones". Esta
-- funcion debe aceptar una "ListaTransformaciones" y un vector
-- de 2 elementos (matriz de 2X1). Esta funcion debe aplicar
-- todas las tranformaciones en la lista una despues de la otra
-- actualizando el resultado cada vez que se aplica una
-- transformacion.
aplicarTransformaciones :: ListaTransformaciones -> (Float, Float) -> (Float, Float)
aplicarTransformaciones NilMatrix (x, y) = (x, y)
aplicarTransformaciones (ConsMatrix ((m11, m12), (m21, m22)) ns) (x, y) = aplicarTransformaciones ns ((m11 * x) + (m12 * y), (m21 * x) + (m22 * y)) 
-- aplicarTransformaciones (ConsMatrix ((5,5), (10,10)) (ConsMatrix ((2,2), (4,4)) NilMatrix)) (1, 2)
-- |ns = (ConsMatrix ((2,2), (4,4)) NilMatrix))| |(x, y) = (1, 2)| |m11 = 5, m12 = 5, m21 = 10, m22 = 10|
-- aplicarTransformaciones (ConsMatrix ((2,2), (4,4)) NilMatrix)) ((5 * 1) + (5 * 2), (10 * 1) + (10 * 2))
-- aplicarTransformaciones (ConsMatrix ((2,2), (4,4)) NilMatrix)) (15, 30)
-- |ns = NilMatrix| |(x, y) = (15, 30)| |m11 = 2, m12 = 2, m21 = 4, m22 = 4|
-- aplicarTransformaciones NilMatrix ((2 * 15) + (2 * 30), (4 * 15) + (4 *30))
-- aplicarTransfoormaciones NilMatrix (90, 180)
-- Linea 46 especifica que la transformacion de una lista vacía y una matriz de 2 x 1 es igual a la misma matriz,
-- por lo que la transformación completa acabaría en la respuesta (90, 180)

-- Problema #4
-- Defina una funcion llamada "sonIguales". Esta funcion
-- toma dos valores de tipo "Lista". Debe retornar "True" si
-- ambas listas tienen los mismos valores en la misma posicion.
sonIguales :: Lista -> Lista -> Bool
sonIguales Nil Nil = True  
sonIguales Nil (Cons t ts) = False 
sonIguales (Cons t ts) Nil = False 
sonIguales (Cons t ts) (Cons m ms) = t == m && sonIguales ts ms
--sonIguales (Cons 1 (Cons 2 Nil)) (Cons 1 (Cons 2 (Cons 3 Nil)))
-- |t = 1, ts = Cons 2 Nil| |m = 1, ms = Cons 2 (Cons 3 Nil)|
-- sonIguales (Cons 2 Nil) (Cons 2 (Cons 3 Nil))
-- |t = 2, ts = Nil| |m = 2, ts = Cons 3 Nil|
--sonIguales (Nil) (Cons 3 Nil)
-- Linea 55 especifica que una lista vacía no es igual a ninguna lista con algún entero, por lo que es False
--sonIguales (Cons 1 (Cons 2 (Cons 3 Nil))) (Cons 1 (Cons 2 (Cons 3 Nil)))
-- |t = 1, ts = (Cons 2 (Cons 3 Nil))| |m = 1, ms = (Cons 2 (Cons 3 Nil)|
-- sonIguales (Cons 2 (Cons 3 Nil)) (Cons 2 (Cons 3 Nil))
-- |t = 2, ts = (Cons 3 Nil)| |m = 2, ms = (Cons 3 Nil)
-- sonIguales (Cons 3 Nil) (Cons 3 Nil)
-- |t = 3, ts = Nil| |m = 3, ms = Nil|
-- sonIgales Nil Nil
-- Linea 54 especifica que 2 listás vacías son iguales, por lo que es True

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
sonEquivalentes m Nil Nil = True  
sonEquivalentes m Nil (Cons n ns) = False  
sonEquivalentes m (Cons n ns) Nil = False  
sonEquivalentes m (Cons n ns) (Cons t ts)= mod n m  == mod t m && sonEquivalentes m ns ts
-- sonEquivalentes 5 (Cons 1 (Cons 2 Nil)) (Cons 6 (Cons 7 Nil))
-- |m = 5| |n = 1, ns = Cons 2 Nil| |t = 6, ts = Cons 7 Nil|
-- |mod 1 5 = 1| |mod 6 5 = 1| = True
-- sonEquivalentes 5 (Cons 2 Nil) (Cons 7 Nil)
-- |m = 5| |n = 2, ns = Nil| |t = 7, ts = Nil|
-- |mod 2 5 = 2| |mod 7 5 = 2| = True
-- sonEquivalentes 5 Nil Nil
-- Linea 96 especifica que el modulo de dos listas vacías son equivalentes, por lo que es True
-- sonEquivalentes 3 (Cons 1 (Cons 2 Nil)) (Cons 6 (Cons 7 Nil))
-- |m = 3| |n = 1, ns = Cons 2 Nil| |t = 6, ns = Cons 7 Nil|
-- |mod 1 3 = 1| |mod 6 3 = 0| = False
-- En el momento en el que mod de n m no es igual a mod de t m la ecuación se vuelve false ya
-- que no son equivalentes

main = undefined