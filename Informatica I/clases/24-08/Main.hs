{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (
    Int,
    Show,
    (-),
    (+),
    undefined,
    (*),
    div
    )

-- 1 -> (2 -> (3 -> (4 -> _ ) ) )

-- 1 -> (2 -> 3 -> _)
-- n = 1
-- ns = (2 -> 3 -> _)
-- 1) reversa ns = (3 -> 2 -> _)
-- 2) agregar n al final
-- 3) (3 -> 2 -> 1 -> _)

data Nat = Cero | Succ Nat

-- 1 := Succ Cero
-- 2 := Succ (Succ Cero)

-- Int := (-2^63 - 2^63)
data Lista = Cons Int Lista | Nil deriving Show

-- range 3
-- | n = 3
-- = Cons 3 (range (3 - 1)) = Cons 3 (range 2)
-- | n = 2
-- = Cons 3 (Cons 2 (Range (2 - 1))) = Cons 3 (Cons 2 (Range 1))
-- | n = 1
-- = Cons 3 (Cons 2 (Cons 1 (Range (1 - 1)))) = Cons 3 (Cons 2 (Cons 1 (Range 0)))
-- | n = 0
-- = Cons 3 (Cons 2 (Cons 1 Nil))
range 0 = Nil
range n = Cons n (range (n - 1))

--Range' 3
-- | n = 3
-- = Cons 3 (range' (3 + 1)) = Cons 3 (range' 4)
-- | n = 4
-- = Cons 3 (Cons 4 (range ' (4 + 1))) = Cons 3 (Cons 4 (range ' 5))

range' 0 = Nil
range' n = Cons n (range' (n + 1))

tomar _ Nil = Nil
tomar 0 _ = Nil
tomar n (Cons x xs) = Cons x (tomar (n - 1) xs)

-- agregarFinal 4 (Cons 1 (Cons 2 (Cons 3 Nil)))
-- | x = 4, n = 1, ns = (Cons 2 (Cons 3 Nil))
-- = Cons 1 (agregarFinal 4 (Cons 2 (Cons 3 Nil)))
-- | x = 4, n = 2, ns = Cons 3 Nil
-- = Cons 1 (Cons 2 (agregarFinal 4 (Cons 3 Nil)))
-- | x = 4, n = 3, ns = Nil
-- = Cons 1 (Cons 2 (Cons 3 (agregarFinal 4 Nil)))
-- | x = 4
-- = Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))
agregarFinal x Nil = Cons x Nil
agregarFinal x (Cons n ns) = Cons n (agregarFinal x ns)

-- contar (Cons 1 (Cons 2 (Cons 3 Nil)))
contar Nil = 0
contar (Cons n ns) = 1 + contar ns

reversa Nil = Nil
reversa (Cons n ns) =
    agregarFinal n (reversa ns)

main = undefined
