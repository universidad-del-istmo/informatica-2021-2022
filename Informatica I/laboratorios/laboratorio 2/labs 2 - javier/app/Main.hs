{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude ( IO, Show, undefined,)

data Natural = O | Succ Natural deriving Show

O + m = m
n + O = n
n + (Succ a) = Succ (n + a)

-- multiplicacion
n * O = O
n * Succ O = n
Succ O * b = b 
n * Succ a = n + (n * a)

-- fibonacci
fib 0 = 0 
fib (Succ 0) = Succ 0 
fib (Succ(Succ a)) = fib (Succ a) + fib a  


-- son iguales 
sonIguales O O = Succ O
sonIguales O m = O
sonIguales n O = O
sonIguales (Succ a) (Succ b) = sonIguales a b

-- factorial 

factorial O = Succ O 
factorial (Succ O) = Succ O
factorial (Succ(Succ a)) = Succ(Succ a) * factorial (Succ a) 

main :: IO()
main = undefined


-- Javier Celada 
-- Informatica 1 
-- FING - seccion B 