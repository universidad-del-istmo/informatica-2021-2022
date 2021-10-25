{-# LANGUAGE NoImplicitPrelude #-}

module Main where

<<<<<<< HEAD
import Prelude (Int, Show, (+), (*), (-), undefined)
=======
import Prelude (Int, Show, (+), (*), (-), fst, undefined)
>>>>>>> e8e7cfa7d9e2e07ea96f27e5db339a50d5f80995

data Lista = Nil | Cons Int Lista deriving Show

lista1 = Cons 1 (Cons 2 (Cons 3 Nil))

sum Nil = 0
sum (Cons x xs) = x + sum xs

multiplicatoria Nil = 0
multiplicatoria (Cons x xs) = x * multiplicatoria xs

pushBack x Nil = Cons x Nil
pushBack x (Cons y ys) = Cons y (pushBack x ys)

take _ Nil = Nil
take 0 _ = Nil
take n (Cons x xs) = Cons x (take (n - 1) xs)

reverse Nil = Nil
reverse (Cons x xs) = pushBack x (reverse xs)

fold :: (estado -> Int -> estado) -> estado -> Lista -> estado
fold agg cero Nil = cero
fold agg cero (Cons x xs) =
    agg (fold agg cero xs) x

sumAgregador estado x = estado + x

-- sum' (Cons 1 (Cons 2 Nil))
-- | xs = (Cons 1 (Cons 2 Nil))
-- = fold sumAgregador 0 xs [xs/Cons 1 (Cons 2 Nil)]
-- = fold sumAgregador 0 (Cons 1 (Cons 2 Nil))
-- | fold: cons = sumAgregador, nil = 0, x = 1, xs = Cons 2 Nil
-- = cons (fold cons nil xs) x [cons/sumAgregador, nil/0, x/1, xs/Cons 2 Nil]
-- = sumaAgregador (fold sumaAgregador 0 (Cons 2 Nil)) 1
-- | fold: cons = sumaAgregador, nil = 0, x = 2, xs = Nil
-- = sumaAgregador (cons (fold cons nil xs) x [cons/sumaAgregador, nil/0, x/2, xs/Nil]) 1
-- = sumaAgregador (sumaAgregador (fold sumaAgregador 0 Nil) 2) 1
-- | fold: cons = sumaAgregador, nil = 0
-- = sumaAgregador (sumaAgregador (nil [cons/sumaAgregador, nil/0]) 2) 1
-- = sumaAgregador (sumaAgregador 0 2) 1
-- | sumaAgregador: estado = 0, x = 2
-- = sumaAgregador (estado + x [estado/0, x/2]) 1
-- = sumaAgregador (0 + 2) 1 = sumaAgregador 2 1
-- | sumaAgregador: estado = 2, x = 1
-- = estado + x [estado/2, x/1]
-- = 2 + 1 = 3
sum' xs = fold sumAgregador 0 xs

pushAgregador estado x = Cons x estado

-- Hacer la reducion de "pushBack 3 (Cons 1 (Cons 2 Nil))"
pushBack' x xs =
    fold pushAgregador (Cons x Nil) xs

reverseAgregador estado x = pushBack' x estado

reverse' xs = fold reverseAgregador Nil xs

-- Utilizar fold para definir "take"

<<<<<<< HEAD
=======
dropAgg (resultado, 0) x = (pushBack x resultado, 0)
dropAgg (resultado, n) x = (resultado, n - 1)

dropAux n xs = fold dropAgg (Nil, n) xs

drop n xs = fst (dropAux n (reverse xs))

>>>>>>> e8e7cfa7d9e2e07ea96f27e5db339a50d5f80995
main = undefined
