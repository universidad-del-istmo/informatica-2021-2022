{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import Prelude (IO, Show, undefined)


data Natural = O | Succ Natural deriving Show

O + m = m
n + O = n
n + (Succ a) = Succ (n + a)

--Ejercicio #1: Multiplicacion inductiva con Haskell
--Defina la operacion multiplicacion de numeros de peano utilizando haskell.
--Puede utilizar el archivo adjunto como base y agregar en el su definicion al final.
--Puede utilizar la definicion para suma que tambien esta disponible en el archivo "Main.hs"

n * O = O
n * Succ O = n
n * Succ a = n + (n * a)

uno = Succ O
dos = Succ uno 
tres = Succ dos
cuatro = Succ tres
cinco = Succ cuatro 
seis = Succ cinco 
siete = Succ seis
ocho = Succ siete
nueve = Succ ocho

--Ejercicio #2: Induccion
--El factorial es una funcion definida como:
--n! = n * (n-1)*(n-2)*...*2*1
factorial O = Succ O 
factorial (Succ O) = Succ O
factorial (Succ(Succ a)) = Succ(Succ a) * factorial (Succ a) 

main :: IO ()
main = undefined