{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (IO, undefined)

data Natural = Cero | Succ Natural

--Laboratorio #2 JuanPabloZebadúaEngel
--Ejercicio 1
--suma

O + m = m
n + O = n
n + (Succ a) = Succ (n + a)

--Multiplicación
n * O = O
n * Succ O = N
n * Succ a = n + (n * a)

--Ejercicio 2
--Regla de fibonachi
fib O = O 
fib (Succ O) = Succ O
fib Succ(Succ a) = fib (Succ a) + fib a

-- factorial n = n * factorial (n - Succ O)

factorial O O = Succ O
factorial O m = O
factorial n O = O
factorial (Succ a) (Succ b) = factorial a b


uno = Succ 0

dos = Succ uno

tres = Succ dos

cuatro = Succ tres

fib O = O
fib (Succ O) = Succ O
fib (Succ (Succ a)) = fib (Succ a) + fib a 


main :: IO ()
main = undefined
