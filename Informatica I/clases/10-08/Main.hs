{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Show, undefined)

data Natural = O | Succ Natural deriving Show

O + m = m
n + O = n
n + (Succ a) = Succ (n + a)

-- factorial n = n * factorial (n - Succ O)

sonIguales O O = Succ O
sonIguales O m = O
sonIguales n O = O
sonIguales (Succ a) (Succ b) = sonIguales a b

-- sonIguals (Succ (Succ O)) (Succ (Succ O))
-- | a = Succ O, b = Succ O
-- = sonIguales (Succ O) (Succ O)
-- | a = O, b = O
-- = sonIguales O O
-- = Succ O

uno = Succ O

dos = Succ uno

tres = Succ dos

cuatro = Succ tres

fib O = O
fib (Succ O) = Succ O
fib (Succ (Succ a)) = fib (Succ a) + fib a 

main = undefined
