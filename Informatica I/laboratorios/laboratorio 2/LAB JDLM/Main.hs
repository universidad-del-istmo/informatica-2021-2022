--Laboratorio #2 13/08/21 Grupo ME, JDLM
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Show, undefined)

data Natural = O | Succ Natural deriving Show

--Ejercicio 1 Multiplicación inductiva con Haskell
--suma

O + m = m
n + O = n
n + (Succ a) = Succ (n + a)

--Multiplicación
n * O = O
n * Succ O = N
n * Succ a = n + (n * a)

--Regla de fibonachi
fib O = O 
fib (Succ O) = Succ O
fib Succ(Succ a) = fib (Succ a) + fib a

-- factorial n = n * factorial (n - Succ O)

factorial O O = Succ O
factorial O m = O
factorial n O = O
factorial (Succ a) (Succ b) = factorial a b

-- factorial (Succ (Succ O)) (Succ (Succ O))
-- | a = Succ O, b = Succ O
-- = factorial (Succ O) (Succ O)
-- | a = O, b = O
-- = factorial O O
-- = Succ O

uno = Succ O

dos = Succ uno

tres = Succ dos

cuatro = Succ tres

fib O = O
fib (Succ O) = Succ O
fib (Succ (Succ a)) = fib (Succ a) + fib a 

main = undefined