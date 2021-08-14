{-#LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (IO, Show, undefined)

data Natural = Cero | Succ Natural deriving Show

Cero + m = m
n + Cero = n
n + (Succ a) = Succ (n + a)

--Ejercicio 1 

--Cero * m = Cero 
--n * Cero = Cero
--m * Succ (Cero) = m
--n * Succ (Cero) = n
--m * n = n * m
--n * n = n + n
--m * m = m + m
--m * Succ(n) = m + Succ(n * m)



main :: IO ()
main undefinied
