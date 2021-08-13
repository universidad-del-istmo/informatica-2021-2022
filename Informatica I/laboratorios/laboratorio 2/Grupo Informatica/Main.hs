{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import Prelude (IO, Show, undefined)

data Natural = O | Succ Natural deriving Show

O + m = m
n + O = n
n + Succ a = Succ (n + a)

-- Ejercicio #1: Definición de la operación de multiplicación utilizando
-- Haskell.
n * O = O
O * m = O
n * Succ m = n + (n * m)

--Ejercicio #2: Definición de un factorial utilizando Haskell.
factorial O = Succ O
factorial (Succ n) = Succ n * factorial n

-- Factorial n = n * factorial (n - 1)
-- Factorial (Succ n) = Succ n * factorial n
-- Demostración:
-- Factorial (Succ (Succ (O)) = Succ (Succ (O)) * Factorial (Succ O)
--                            = Succ (Succ (O)) * Succ O * factorial O
--                            = Succ (Succ (O)) * Succ O * Succ O
--                            = Succ (Succ (O))

main :: IO ()
main = undefined
