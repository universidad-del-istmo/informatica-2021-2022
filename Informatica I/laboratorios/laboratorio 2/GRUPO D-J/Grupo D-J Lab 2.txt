{-# LANGUAGE NoImplicitPrelude #-}


module Main where

import Prelude (Show, undefined, Integer)


data Natural = O | Succ Natural deriving Show


uno = Succ O
dos = Succ uno
tres = Succ dos
cuatro = Succ tres

-- Definicion suma -- 

O + m = m
n + O = n
n + Succ a = Succ (n + a)


-- multiplicacion inductiva --

n * O = O
O * m = O
n * Succ m = n + ( n * m )

-- Factorial --

factorial O = Succ O
factorial (Succ n) = Succ n * factorial n 


main = undefined