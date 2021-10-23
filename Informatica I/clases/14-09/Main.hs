{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Float, Int, Show, (+), (-), undefined)

data ListaI = NilI | ConsI Int ListaI deriving Show

data ListaV = NilV | ConsV (Int, Int) ListaV deriving Show

data ListaV3 = NilV3 | ConsV3 (Float, Float, Float) ListaV3 deriving Show

data Lista a = Nil | Cons a (Lista a) deriving Show

-- take es una funcion polimorfica que
-- funciona con cualquier tipo de lista
take :: Int -> Lista a -> Lista a
take n Nil = Nil
take 0 lista = Nil
take n (Cons x xs) = Cons x (take (n - 1) xs)

takeI n NilI = NilI
takeI 0 lista = NilI
takeI n (ConsI x xs) = ConsI x (takeI (n - 1) xs)

takeV n NilV = NilV
takeV 0 lista = NilV
takeV n (ConsV x xs) = ConsV x (takeV (n - 1) xs)

drop :: Int -> Lista a -> Lista a
drop 0 lista = lista
drop n Nil = undefined
drop n (Cons x xs) = drop (n - 1) xs

map :: (a -> b) -> Lista a -> Lista b
map f Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

mapV f NilV = NilV
mapV f (ConsV x xs) = ConsV (f x) (mapV f xs)

incrementar :: Lista Int -> Lista Int
incrementar Nil = Nil
incrementar (Cons x xs) = Cons (x + 1) (incrementar xs)

sum :: Lista Float -> Float
sum Nil = 0
sum (Cons x xs) = x + (sum xs)

main = undefined
