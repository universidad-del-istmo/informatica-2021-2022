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

--Multiplicacion
O * m = O
n * O = O 
Succ O * m = m 
n * Succ O = n 
n * Succ m = n + (n * m)

--factorial
--Para usarlo con numeros naturales se debe de colocar en un parentesis: fac (anat 3)
fac O = Succ O
fac (Succ O) = Succ O 
fac (Succ (Succ a)) = Succ (Succ a) * fac (Succ a)
--fac (anat a) = (anat a) * fac (anat (a - 1))
--fac (Succ a) = Succ a * fac(Succ a - 1)

{-
sonIguales O O = Succ O
sonIguales O m = O
sonIguales n O = O
sonIguales (Succ a) (Succ b) = sonIguales a b

fib O = O
fib (Succ O) = Succ O
fib (Succ (Succ a)) = fib (Succ a) + fib a 

--Predecesor
--prednat 0 = O 
--prednat 0 = O 
--prednat 1 = O
--prednat n = Succ (prednat (n - 1))
-}
main = undefined