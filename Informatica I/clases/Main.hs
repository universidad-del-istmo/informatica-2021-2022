--RESOLUCIÓN PRIMER PARCIAL / PABLO MORALES


--Ejercicio 1 

Cero * m = Cero 
n * Cero = Cero
m * Succ (Cero) = m
n * Succ (Cero) = n
m * n = n * m
n * n = n + n
m * m = m + m
m * Succ(n) = m + Succ(n * m)



--Ejercicio 2

a + 0 = a
a + Succ (b) = Succ (a) + b

0 + Succ (b) = Succ (0) + b = Succ (0) + b
0 + Succ (a) = Succ (0) + a = Succ (0) +a

a > b = Succ (a)
a > 0 = Succ (a)
Succ (0) = Succ (a)
b + Succ (0) = Succ (a)
b + Succ (0) = Succ (a+b)



--Ejercicio 3

n par = Succ 0 si n = par
n impar = Succ 0 si n = impar

n es par = Succ 0 +1
n es impar = Succ 0 + 0


--Definición:
es par 0 + 0 = Succ 0
es par 0 + n = Succ 0
es par 0 + (n + 1) = 0
es par 0 + Succ (n) = 0

es impar 0 + 0 = 0
es impar 0 + n = Succ 0
es impar 0 + (n + 1) = 0
es impar 0 + Succ (n) = 0
es impar 0 + Succ (n+1) = Succ 0



--Ejercicio 4

{-#LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (IO, Show, undefined)

data Natural = Cero | Succ Natural deriving Show


Cero + m = m
n + Cero = n
n + (Succ a) = Succ (n + a)


uno = Succ (O)
dos = Succ (uno)
tres = Succ (dos)
cuatro = Succ (cuatro)

Predecesor O = O
predecesor Succ (O) = O
predecesor Succ (Succ (O)) = Succ (O)

predecesor Succ (a) = a
predecesor (Succ (a)) = Succ (a)
predecesor (Succ (Succ (a))) = Succ (Succ (a))


a > b = Succ (0) > 0
main :: IO ()
main undefined
