{-# LANGUAGE NoImplicitPrelude #-}

module Main where

data Natural = Cero | Succ Natural

Cero + m = m
n + Cero = n
n + (Succ a) = Succ (n + a)

factorial Cero = Succ Cero
factorial (Succ a) = -- multiplicar (Succ a) por el factorial de a