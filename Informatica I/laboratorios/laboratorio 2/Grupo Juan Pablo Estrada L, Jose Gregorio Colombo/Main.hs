{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import Prelude (IO, Show, undefined)


data Natural = O | Succ Natural deriving Show

O + m = m
n + O = n
n + (Succ a) = Succ (n + a)

--Trabajo en parejas (Juan Pablo Estrada L, Jose Gregorio Colombo)


--Ejercicio 1 Inducción
--Defina la operaci´on multiplicaci´on de numeros de peano utilizando Haskell. Pede utilizar el archivo adjunto
--como base y agregar en el su definici´on al final. Puede utilizar la definicion para suma que tambien esta
--disponible en el archivo “Main.hs”.

--Reglas de multiplicación
n * O = O
n * Succ O = n
Succ O * b = b 
n * Succ a = n + (n * a)

--Números naturales
uno = Succ O

dos = Succ uno 

tres = Succ dos

cuatro = Succ tres

cinco = Succ cuatro 

seis = Succ cinco 

siete = Succ seis

ocho = Succ siete

nueve = Succ ocho

--Ejercicio 2
--El factoria es una funcion definida como:
--n! = n ∗ (n − 1) ∗ (n − 2) ∗ ... ∗ 2 ∗ 1

--Definición de factorial
--Factorial de cero debe de dar como resultado o ser igual al sucesor de cero 
--El factorial del succ de cero debe ser igual al mismo (Succesor de cero)
factorial O = Succ O 
factorial (Succ O) = Succ O
factorial (Succ(Succ a)) = Succ(Succ a) * factorial (Succ a) 


main :: IO ()
main = undefined 

