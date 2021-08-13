{-# LANGUAGE NoImplicitPrelude #-}


module Main where
import Prelude (Show, undefined)

data Natural = Cero | Succ Natural deriving Show

-- Suma
Cero + m = m
n + Cero = n 
n + (Succ a) = Succ (n + a)

-- Regla multiplicación 
n * Cero = Cero
n * Succ Cero = n
n * Succ a = n + (n * a)

-- regla de fibonachi (apoyo para factorial)
fib Cero = Cero
fib (Succ Cero) = Succ Cero
fib (Succ (Succ a)) = fib (Succ a) +  fib a

-- Regla factorial 
factorial Cero = Succ Cero 
factorial (Succ Cero) = Succ Cero
factorial (Succ (Succ a)) = Succ (Succ a ) * factorial (Succ a)
--factorial (Succ a) = (factorial a) * a

--main :: IO ()
main = undefined