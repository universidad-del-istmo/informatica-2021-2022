{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (IO, Show, undefined)

data Natural = Cero | Succ Natural deriving Show

Cero + m = m
n + Cero = n
n + Succ (a) = Succ (n + a)
m + Succ (a) = Succ (m + a)


n x Cero = Cero
m x Cero = Cero
n x Succ(Cero) = n
m x Succ(Cero) = m
n x m = m x n
n x n = n + n
m x m = m + m
n x Succ(m) = n + Succ(m x n)




main :: IO ()
main = undefined

