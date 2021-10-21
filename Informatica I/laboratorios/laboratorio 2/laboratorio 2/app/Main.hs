{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (IO, Show, undefined)

data Natural = Cero | Succ Natural deriving Show 
 
--Suma--
Cero + m = m
n + Cero = n
n + (Succ a) = Succ (n + a)


--EJERCICIO #1: MULTIPLICACIÓN INDUCTIVA
--Reglas 
Cero * m = Cero 
n * Cero = Cero 
n * Succ Cero = n 
n * Succ m = n + (n * m) 


--DEFINIR FIBONACCI PARA FACTORIALES 
--Fibonacci 
fib Cero = Cero 
fib (Succ Cero) = Succ Cero 
fib (Succ(Succ a)) = fib (Succ a) + fib a 



--EJERCICIO #2: INDUCCIÓN
--Reglas
--factorial n = n * factorial (n-1) 
--factorial(n + Succ Cero) = (n + Succ Cero) * factorial n 
factorial Cero = Succ Cero -- Factorial de Cero
factorial (Succ Cero) = Succ Cero 
factorial (Succ(Succ a)) = Succ(Succ a) * factorial (Succ a)


main :: IO ()
main = undefined
