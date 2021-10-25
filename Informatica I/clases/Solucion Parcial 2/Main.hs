{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Bool, Int, Show, (*), (==), (>=), (||), (&&), (+), (-), lcm, gcd, mod, undefined)

mcd = gcd

mcd3 a b c = mcd a (mcd b c)

mcd3Aux a b c respuesta contador =
    if mod a contador == 0 && mod b contador == 0 && mod c contador == 0
    then mcd3Aux a b c contador (contador + 1)
    else if contador >= a || contador >= b || contador >= c
    then respuesta
    else mcd3Aux a b c respuesta (contador + 1)

mcd3' a b c = mcd3Aux a b c 1 1

mcm = lcm

mcm3 a b c = lcm a (lcm b c)

mcm3Aux a b c respuesta contador =
    if mod contador a == 0 && mod contador b == 0 && mod contador c == 0
    then mcm3Aux a b c contador (contador - 1)
    else if a >= contador || b >= contador || c >= contador
    then respuesta
    else mcm3Aux a b c respuesta (contador - 1)

mcm3' a b c = mcm3Aux a b c (a*b*c) (a*b*c)

data Lista = Nil | Cons Int Lista deriving Show    

divideATodos :: Lista -> Int -> Bool
divideATodos = undefined

mayorOIgualQueAlguno :: Lista -> Int -> Bool
mayorOIgualQueAlguno = undefined

mcdLAux xs respuesta contador =
    if divideATodos xs contador
    then mcdLAux xs contador (contador + 1)
    else if mayorOIgualQueAlguno xs contador
    then respuesta
    else mcdLAux xs respuesta (contador + 1)

mcdL' xs = mcdLAux xs 1 1

mcdL Nil = 0
mcdL (Cons x xs) = gcd x (mcdL xs)

mcmL Nil = 1
mcmL (Cons x xs) = mcm x (mcmL xs)

-- Verifica si el segundo parametro es
-- multiplo de todos los valores de la
-- lista.
esMultiploDeTodos :: Lista -> Int -> Bool
esMultiploDeTodos = undefined

esMenorOIgualQueAlguno :: Lista -> Int -> Bool
esMenorOIgualQueAlguno = undefined

product Nil = 1
product (Cons x xs) = x * product xs

mcmLAux xs respuesta contador =
    if esMultiploDeTodos xs contador
    then mcmLAux xs contador (contador - 1)
    else if esMenorOIgualQueAlguno xs contador
    then respuesta
    else mcmLAux xs respuesta (contador - 1)

mcmL' xs = mcmLAux xs (product xs) (product xs)

fold fCons fNil Nil = fNil
fold fCons fNil (Cons x xs) = fCons (fold fCons fNil xs) x

mcdAgregador mcdCandidato x = mcd x mcdCandidato

mcdL'' xs = fold mcdAgregador 0 xs 

mcmAgregador mcmCandidato x = mcm x mcmCandidato

mcmL'' xs = fold mcmAgregador 1 xs

main = undefined
