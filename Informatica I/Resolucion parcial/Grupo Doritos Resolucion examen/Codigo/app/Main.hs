{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Prelude (Show, undefined, appendFile, (-), Foldable (sum) )

data Natural = O | Succ Natural deriving Show 

--De numeros naturales a numeros de peano
-- Anat 6 = Succ......
anat 0 = O
anat n = Succ (anat (n - 1))

--Succ O = anat 1
--Suma
(+) :: Natural -> Natural -> Natural
O + m = m
n + O = n 
n + (Succ a) = Succ (n + a)

--Predecesor
pred (Succ O) = O
pred (Succ a) = a 
main = undefined 