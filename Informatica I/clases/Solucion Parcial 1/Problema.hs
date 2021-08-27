module Main where

data Natural = Cero | Succ Natural

predecesor Cero = Cero
predecesor (Succ a) = a