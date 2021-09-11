{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Float, Bool(..), Show, (+), (-), (==), (||), (*), (>), (&&), (**), lcm, floor, fst, snd, div, mod, undefined)
data Lista = Nil | Cons Int Lista deriving Show

fold :: (estado -> Int -> estado) -> estado -> Lista -> estado
fold agg cero Nil = cero    
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) 




lcm :: Int -> Int -> Int
mcmAux n m z i d
  | i < n || i < m = d || i < z 
  | mod i n == 0 && mod i m == 0 && mod i z == 0 = mcmAux n m z  (i - 1) i
  | otherwise = mcmAux n m z (i - 1) d
mcm n m = mcmAux n m z (n * m) (n * m) (n * z) (z * m)  

lcm  n m  1 = mcdAux n m 1 1

lcm n m z = undefined