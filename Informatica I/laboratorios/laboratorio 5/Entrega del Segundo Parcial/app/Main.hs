{-# LANGUAGE NoImplicitPrelude #-} 

module Main where

import Prelude (Int, Show,Float, Bool(..), div, snd, fst, (-),(+), mod, (||), (*),(/),(^), (&&), (>),(==), undefined)

data Lista = Nil | Cons Int Lista deriving Show

fold :: (estado -> Int -> estado) -> estado -> Lista -> estado
fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x

mcdAux n m i d = 
 if  n == i || m == i
    then d
    else if  mod n (i + 1) == 0 && mod m (i+1) == 0 
    then mcdAux n m (i +1) (i+1)
    else mcdAux n m (i +1) d



mcd n m = mcdAux n m 1 1 


--PUNTO 1 DEL EXAMEN-PREGUNTA 1

mcd3 :: Int -> Int -> Int -> Int
mcd3 a b c = mcd a (mcd b c)

mcdAux2 (Cons x xs) i d = 
  if comparar (Cons x xs) i
    then d 
    else if compararmod (Cons x xs) i 
    then mcdAux2 (Cons x xs) (i+1) (i+1)
    else mcdAux2(Cons x xs) (i+1) d


--PUNTO 2 DEL EXAMEN-PREGUNTA 3

mcdL :: Lista -> Int 
mcdL Nil = 0
mcdL (Cons x xs) = mcdAux2 (Cons x xs) 1 1


comparar :: Lista -> Int -> Bool
comparar Nil i = False 
comparar (Cons x xs) i = x == i || comparar xs i

compararmod Nil i = True
compararmod (Cons x xs) i = mod x (i+1) == 0 && compararmod xs i


--PUNTO 3 DEL EXAMEN-PREGUNTA 5

mcdAgregador resultado x = mcd x resultado 
mcdf xs = fold mcdAgregador 0 xs 

main = undefined
