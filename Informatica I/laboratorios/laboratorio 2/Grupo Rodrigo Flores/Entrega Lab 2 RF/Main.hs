{-# LANGUAGE NoImplicitPrelude #-}

module Main where 

import Prelude (IO, show, undefined, Show)

data Natural = Cero | Succ Natural deriving Show



Cero + m = m
n + Cero = n
n + (Succ a) = Succ (n + a)

--Laboratorio #2 Rodrigo Flores

--Ejercicio #1 Mutiplicación Inductiva con haskell

n * Cero = Cero
Cero * m = Cero
n * Succ Cero = n
Succ Cero * m = m
n * m = m * n

--Ejercicio #2 Induccion 

                        --n! = n * (n -1) * (n -2) * ...


factorial Cero = Succ Cero
factorial (Succ Cero) = Succ Cero
factorial (Succ(Succ x)) = Succ(Succ x) * factorial (Succ x)


main :: IO ()
main = undefined
