{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (Int, Show, (+), (-), (*), div, undefined)

data Lista = Nil | Cons Int Lista deriving Show

-- incValores (Cons 1 (Cons 2 Nil))
-- | x = 1, xs = Cons 2 Nil
-- = Cons (1 + 1) (incValores (Cons 2 Nil)) = Cons 2 (incValores (Cons 2 Nil))
-- | x = 2, xs = Nil
-- = Cons 2 (Cons (2 + 1) (incValores Nil)) = Cons 2 (Cons 3 (incValores Nil))
-- = Cons 2 (Cons 3 Nil)
incValores Nil = Nil
incValores (Cons x xs) = Cons (x + 1) (incValores xs)

decValores Nil = Nil
decValores (Cons x xs) = Cons (x - 1) (decValores xs)

dupValores Nil = Nil
dupValores (Cons x xs) = Cons (x*2) (dupValores xs)

exp2Valores Nil = Nil
exp2Valores (Cons x xs) = Cons (x*x) (exp2Valores xs)

exp3Valores Nil = Nil
exp3Valores (Cons x xs) = Cons (x*x*x) (exp3Valores xs)

map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

inc x = x + 1

-- incValores (Cons 1 (Cons 2 Nil))
-- = map inc (Cons 1 (Cons 2 Nil))
-- | map: f = inc, x = 1, xs = Cons 2 Nil
-- = Cons (inc 1) (map inc (Cons 2 Nil))
-- | inc: x = 1
-- = Cons (1 + 1) (map inc (Cons 2 Nil)) = Cons 2 (map inc (Cons 2 Nil))
-- | map: f = inc, x = 2, xs = Nil
-- = Cons 2 (Cons (inc 2) (map f Nil))
-- | inc: x = 2
-- = Cons 2 (Cons (2 + 1) (map inc Nil)) = Cons 2 (Cons 3 (map inc Nil))
-- = Cons 2 (Cons 3 Nil)
incValores' xs = map inc xs

dec x = x - 1

decValores' xs = map dec xs


main = undefined 