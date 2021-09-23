{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Float, Bool(..), Show, (+), (-), (==), (||), (*), (>), (&&), (**), or, and, floor, fst, snd, div, mod, minimum, undefined)

data Lista = Nil | Cons Int Lista deriving Show


mcdAux n m i d =
    if n == i || m == i
    then d
    else if mod n (i + 1) == 0 && mod m (i + 1) == 0
    then mcdAux n m (i + 1) (i + 1)
    else mcdAux n m (i + 1) d

mcd :: Int -> Int -> Int
mcd n m = mcdAux n m 1 1

--1er problema, mcd con 3 digitos
mcd3 a b c = mcd a (mcd b c)

--reduccion
-- mcd3 2 4 6
-- mcd 2 (mcd 4 6)
-- mcd 2 (mcdAux 4 6 1 1)
-- mcd 2 (mod 4 (1 + 1) == 0 && mod 6 (1 + 1))
-- mcd 2 (mod 4 (2) == 0 && mod 6 (2))
-- mcd 2 (0 == 0 && 0 ==0)  |true|
-- mcd 2 (mcdAux 4 6 (2) (2))
-- mcd 2 (mod 4 (2 + 1) == 0 && mod 6 (2 + 1))  |false|
-- mcd 2 (mcdAux 4 6 (3) 2)
-- mcd 2 (mod 4 (3 + 1) == 0 && mod 6 (3 + 1))  |false|
-- mcd 2 (mcdAux 4 6 (4) 2)
-- mcd 2 (4 == 4 || 6 == 4) |true|
-- mcd 2 (2)
-- mcdAux 2 2 1 1
-- mod 2 (1 + 1) == 0 && mod 2 (1 + 1) == 0  |true|
-- mcdAux 2 2 (2) (2)
-- mod 2 (2 + 1) == 0 && mod 2 (2 + 1) == 0  |false|
-- mcdAux 2 2 (2) 2 
-- 2 == 2 || 2 == 2  |true|
-- 2 

--mcdAux2 :: Lista -> Int -> Int -> Int 



--2do problema mcd con una lista de numeros

mcdAux2 (Cons x xs) i d =
    if comparar (Cons x xs) i      
    then d
    else if compararmod (Cons x xs) i 
    then mcdAux2 (Cons x xs) (i + 1) (i + 1)
    else mcdAux2 (Cons x xs) (i + 1) d

mcdlista :: Lista -> Int 
mcdlista Nil = 0
mcdlista (Cons x xs) = mcdAux2 (Cons x xs) 1 1

comparar :: Lista -> Int -> Bool 
comparar Nil i = False 
comparar (Cons x xs) i = or [x == i, (comparar xs i)]

compararmod :: Lista -> Int -> Bool
compararmod Nil i = True 
compararmod (Cons x xs) i = mod x (i + 1) == 0 && compararmod xs i

-- 3er problema mcd con listas pero definirla con fold
fold :: (estado -> Int -> estado) -> estado -> Lista -> estado
--fold funcion intercala lista
fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x

mcdfold :: Lista -> Int
mcdfold (Cons x xs) = mcd x (fold mcd 0 xs)


{- mcd con euclides
prueba :: Int -> Int -> Int
prueba a 0 = a
prueba a b = mcd b (mod b a)
-}
{-
contar :: Lista -> Int
contar Nil = 0
contar (Cons n ns) = 1 + contar ns    -}

main = undefined