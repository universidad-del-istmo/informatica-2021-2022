{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (undefined, abs, Int, sqrt, (<), (<=), (/), (^), (+), (-), div, mod, (*), (==), (/=), (>), (>=), Bool(..), (&&), (||))

esPrimoAuxiliar n contador =
    div n 2 == contador
    || (
        mod n contador /= 0
        && esPrimoAuxiliar n (contador + 1)
    )

esPrimo 0 = False
esPrimo 1 = True
esPrimo 2 = True
esPrimo n = esPrimoAuxiliar n 2

-- esPrimo 71
-- esPrimoAux 71 2
-- esPrimoAux 71 3
-- esPrimoAux 71 4
-- esPrimoAux 71 5
-- esPrimoAux 71 6
-- ..
-- esPrimoAux 71 68
-- esPrimoAux 71 69
-- esPrimoAux 71 70
-- esPrimoAux 71 71

-- 5
-- 4       *
-- 3
-- 2
-- 1 *
-- 0 1 2 3 4 5

distanciaEuclideana (x1, y1) (x2, y2) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^2)

distanciaEuclideana3d (x1, y1, z1) (x2, y2, z2) =
    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2 + (z2 - z1) ^ 2)

sumaVector (x1,y1) (x2, y2) = (x1 + x2, y1 + y2)

sumaEsIgual r f1 f2 = abs ((f1 + f2) - r) <= 0.00001

main = undefined
