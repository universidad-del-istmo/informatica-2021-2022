{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import Prelude (IO, Show, undefined)

data Natural = O | Succ Natural deriving Show

O + m = m
n + O = n
n + Succ a = Succ (n + a)

--Ejercicio #1: Definición de la operación de multiplicación utilizando Haskell.

--Reglas de multiplicación
n * O = O
O * m = O
n * Succ m = n + (n * m)


--Naturales
uno = Succ O
dos = Succ uno 
tres = Succ dos
cuatro = Succ tres



--Ejercicio #2: Definición de un factorial utilizando Haskell.

--El factoria es una funcion definida como:
--n! = n ∗ (n − 1) ∗ (n − 2) ∗ ... ∗ 2 ∗ 1

factorial O = Succ O 
factorial (Succ O) = Succ O
factorial (Succ(Succ a)) = Succ(Succ a) * factorial (Succ a) 

main :: IO ()
main = undefined