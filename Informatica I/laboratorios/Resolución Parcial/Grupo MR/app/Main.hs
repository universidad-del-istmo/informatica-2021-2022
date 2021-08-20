{-# LANGUAGE NoImplicitPrelude #-}


module Main where
import Prelude (Show, undefined)

data Natural = Cero | Succ Natural deriving Show

-- Suma
Cero + m = m
n + Cero = n 
n + (Succ a) = Succ (n + a)

-- Regla multiplicación 
n * Cero = Cero
n * Succ Cero = n
n * Succ a = n + (n * a)


--predecesor de un número 
predecesor Cero = Cero 
predecesor (Succ Cero)= Cero 
predecesor (Succ a)= a

--main :: IO ()
main = undefined
