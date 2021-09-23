{-# LANGUAGE NoImplicitPrelude  #-}


module Main where

import Prelude (IO, show, undefined, Show)

data Natural = Cero | Succ Natural deriving Show

--suma
Cero + m = m
n + Cero = n
n + (Succ a) = Succ (n + a)

--multiplicación
n * Cero = Cero
n * Succ Cero = n
n * (Succ a) = n + (n * a)

sonIguales Cero Cero = Succ Cero
sonIguales Cero n = Cero
sonIguales n Cero = Cero
sonIguales (Succ a) (Succ b) = sonIguales a b

--Fibonaci para factorial
fib Cero = Cero
fib (Succ Cero) = Succ Cero
fib (Succ (Succ a)) = fib (Succ a) + fib a

--factorial
factorial Cero = Succ Cero
factorial (Succ Cero) = Succ Cero
factorial (Succ(Succ a)) = Succ(Succ a) * factorial (Succ a)

-- predecesor 

predecesor Cero = Cero
predecesor (Succ Cero) =  Succ Cero
predecesor (Succ(Succ a)) = Succ a + Cero


main :: IO ()
main = undefined


