{-# LANGUAGE NoImplicitPrelude #-}

module Main where

data Natural = Cero | Succ Natural

Cero + m = m
n + Cero = n
n + (Succ a) = Succ (n + a)