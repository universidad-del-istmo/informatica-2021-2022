{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (undefined, Int, (^), (+), (-), div, mod, (*), (==), (/=), (>), (>=), Bool(..), (&&), (||))

unNumero :: Int
unNumero = 2^61

otroNumero = unNumero + 42

resultadoDivision = div 10 3

resultadoDivisionRes = mod 10 3

resultadoMult = 43 * 23

resultadoResta = 44 - 322

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

esPar n = mod n 2 == 0

unBool :: Bool
unBool = True

otroBool :: Bool
otroBool = False

esDivisibleEntreCinco n = mod n 5 == 0

esDivisibleEntreDosyCinco n = esDivisibleEntreCinco n && esPar n

esDivisibleEntreDosOCinco n = esDivisibleEntreCinco n || esPar n

esMayorQue10 n = n > 10

esMayorOIgualQue42 n = n >= 42

esPrimoAuxiliar n contador = n == contador || (mod n contador /= 0 && esPrimoAuxiliar n (contador + 1))

esPrimo 0 = False
esPrimo 1 = True
esPrimo 2 = True
esPrimo n = esPrimoAuxiliar n 2

-- esPrimo 5 = esPrimoAuxiliar 5 2
-- = 5 == 2 || (mod 5 2 /= 0 && esPrimoAuxiliar 5 (2 + 1))
-- = False || (1 /= 0 && esPrimoAuxiliar 5 3)
-- = False || (True && esPrimoAuxiliar 5 3)
-- = False || (True && (5 == 3 || (mod 5 3 /= 0 && esPrimoAuxiliar 5 (3+1)))
-- = False || (True && (False || (2 /= 0 && esPrimoAuxiliar 5 4))
-- = False || (True && (False || (True && esPrimoAuxiliar 5 4))
-- = False || (True && (False || (True && (5 /= 4 || (mod 5 4 /= 0 && esPrimoAuxiliar 5 (4 + 1))))
-- = False || (True && (False || (True && (True || (1 /= 0 && esPrimoAuxiliar 5 5)))
-- = False || (True && (False || (True && (True || (False && (5 == 5 || (mod 5 5 /= 0 && esPrimoAuxiliar 5 (5 + 1))))
-- = True && (False || (True && (False || (True && (5 == 5 || (mod 5 5 /= 0 && esPrimoAuxiliar 5 (5 + 1)))
-- = False || (True && (False || (False && (5 == 5 || (mod 5 5 /= 0 && esPrimoAuxiliar 5 (5 + 1))
-- = True && (False || (False && (5 == 5 || (mod 5 5 /= 0 && esPrimoAuxiliar 5 (5 + 1)
-- = (False || (True && (5 == 5 || (mod 5 5 /= 0 && esPrimoAuxiliar 5 (5 + 1)
-- = True && (5 == 5 || (mod 5 5 /= 0 && esPrimoAuxiliar 5 (5 + 1)
-- = 5 == 5 || (mod 5 5 /= 0 && esPrimoAuxiliar 5 (5 + 1)
-- = True || (mod 5 5 /= 0 && esPrimoAuxiliar 5 (5 + 1)
-- = True



main = undefined