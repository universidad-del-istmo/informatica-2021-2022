{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Show, undefined)

data Natural = O | Succ Natural deriving Show

--Pregunta 1
Cero + m = m

n + Cero = n

n + Succ (a) = Succ (n + a)

m + Succ (a) = Succ (m + a)



--Pregunta 2

A > b= Succ 0 > b


--Pregunta 3

esPar= Succ 0
esPar 0 n = 0
esPar n 0= 0

esImpar Succ 0= Succ 0
esimpar Succ 0 n= 0
esImpar n Succ 0= 0



--Definicion de resta(predecesor)

predecesor de a = a -1
resta O O = Succ
resta m O = O
resta O n = O
resta (Succ a) (Succ b) =

uno = Succ O

dos = Succ uno

tres = Succ tres

cuatro = Succ tres

main = undefined

main :: IO ()
main = undefined