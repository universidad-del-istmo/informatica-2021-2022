{-# LANGAUGE NolmplicitPrelude #-}



module Main where

import Prelude (IO, show, undefined )

data Natural = O | Succ Natural deriving Show 


uno = Succ O
dos = Succ uno
tres = Succ dos
cuatro = Succ tres


-- definicion de suma --
O + n = n
m + O = m
n + Succ m = Succ( n + m )

-- parte 1 multplicacion-- 

n * O = O
O * m = O
n* Succ m = n + (n * m)

--parte 2 Factorial-- 


factorial O = Succ O 
factorial (Succ n) = Succ n * factorial n



main :: IO ()

main = undefined 
