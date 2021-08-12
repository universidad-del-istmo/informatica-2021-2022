{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude  (Io, show, undefined)
data Natural = Cero | Succ Natural deriving show 

-- suma --
(+):: Natural -> Natural -> Natural 
Cero + m = n 
n + Cero = n 
n + (Succ a) =Succ(n + a)

-- EJERCICIO 1: MULTIPLICACION INDUCTIVA 
-- Reglas 
(+):: Natural -> Natural -> Natural
cero * n = Cero 
n * Cero = Cero 
n * Succ Cero  = n 
n * Succ m = n + (n * m)
-- Colocamos 4 reglas ya que consideramos que algunas reglas esran redundantes--

-- EJERCICIO 2: INDUCCION 
--REGLAS 
-- Factorial n = n * factorial (n-1)
-- Factorial(n + succ cero)* (n + succ cero) * Factorial n 
factorial Cero = Succ Cero -- REGLA 
factorial (Succ Cero) = Succ Cero
factorial (Succ(Succ a)) Succ(Succ a) * factorial (Succ a)

main :: IO ()
main = putStrLn "Hello, Haskell!"
main = undefined


