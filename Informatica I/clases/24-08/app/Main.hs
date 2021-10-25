{-#LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (
    Int,
    Show,
    (-),
    undefined
    )
import Language.Haskell.TH (Type(PromotedNilT))
import System.Win32 (disableThreadLibraryCalls)

-- 1 -> (2 -> (3 -> (4 -> _) ) )

-- 1 -> (2 -> 3 -> _)
-- n = 1
-- ns = (2 -> 3 -> _)
-- 1) reversa ns = (3 -> 2 -> _)
-- 2) agregar n al final 
-- 3) (3 -> 2 -> 1 -> _)

data Lista = Cons Int Lista | Nil derving show 

range 0 = Nil
range n = Cons n (range (n -1))

-- agregarFinal 4 ((Cons 1 (Cons 2 (Cons 3 Nil)))
-- | x = 4, n = 1, ns = (Cons 2 (cons 3 Nil))
-- = Cons 1 (agregar Final 4 (Cons 2 (Cons 3 Nil)))
-- | x = 4, | n = 2, ns = Cons 3 Nil
-- = Cons 1 (Cons 2 (agregarFinal 4 (Cons 3 Nil)))
-- | x = 4, n = 3, ns = Nil
-- Cons 1 (Cons 2 (Cons 3 (agregarFinal 4 Nil)))
-- | x = 4 
-- Cons 1 (Cons 2 (Cons 3 (Cons 4 Nil)))

agregarFinal x Nil = Cons x Nil
agregarFinal x (Cons n ns) = Cons n (agregarFinal x ns)



-- contar (Cons 1 (Cons 2 (Cons 3 Nil)))

-- | n = 1, ns = (Cons 2 (Cons 3 Nil))

-- 1 + Contar (Cons 2 (Cons 3 Nil)))

-- | n = 2, ns = (Cons 3 Nil) 

-- 1 + 1 + Contar (Cons 3 Nil)

-- | n = 3, ns = Nil 

-- 1 + 1 + 1 + Contar (Nil)

-- |n = Nil

-- 1 + 1 +1 + 0

-- 3

contar Nil = 0 
contar (Cons n ns) = 1 + contar ns


reversa Nil = Nil 
reversa (Cons n ns) =
    agregarFinal n (reversa ns)


data Lista = Cons Int Lista | Nil deriving Show

main :: IO ()
main = undefined 
