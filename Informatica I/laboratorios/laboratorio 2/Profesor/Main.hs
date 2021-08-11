{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (IO, undefined)

data Natural = Cero | Succ Natural

Cero + m = m
n + Cero = n
n + (Succ a) = Succ (n + a)



main :: IO ()
main = undefined
