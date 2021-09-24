{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (undefined, Int, (^), sqrt, abs, (+), div, mod, (*), (-), (/=), (==), Bool(..), (&&), (||), (>), (<), (>=), (<=), Float, sin, cos, pi, (/)) 



unNumero :: Int -- (-2^63, 2^63)
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

esMayorIgualQue42 n = n >= 42

esPrimoAuxiliar n contador = div n 2 == contador || (mod n contador /= 0 && esPrimoAuxiliar n (contador + 1))

esPrimo 0 = False 
esPrimo 1 = True 
esPrimo 2 = True 
esPrimo n = esPrimoAuxiliar n 2

-- esPrimo 71
-- esPrimoAuxiliar 71 2
-- esPrimoAuxiliar 71 3
-- esPrimoAuxiliar 71 4
-- esPrimoAuxiliar 71 5
-- esPrimoAuxiliar 71 6
-- ......
-- esPrimoAuxiliar 71 68
-- esPrimoAuxiliar 71 69
-- esPrimoAuxiliar 71 70
-- esPrimoAuxiliar 71 71

-- esPrimo 5 = esPrimoAuxiliar 5 2 
-- = 5 == 2 || (mod 5 2 /= 0 && esPrimoAuxiliar 5 (2 + 1))
-- = False || (1 /= 0 && esPrimoAuxiliar 5 3)
-- = False || (True && 5 3)
-- = False || (True && (5 == 3) || (mod 5 3 /= 0 && esPrimoAuxiliar 5 (3 + 1)))
-- = False || (True && (False || (2 /= 0 && esPrimoAuxiliar 5 4))
-- = False || 

-- 5
-- 4       *
-- 3
-- 2
-- 1 *
-- 0 1 2 3 4 5

distanciaEuclideana (x1, y1) (x2, y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^2) 

distanciaEuclideana3d (x1, y1, z1) (x2, y2, z2) = sqrt ((x2 - x1) ^2 + (y2 - y1) ^2 + (z2 - z1) ^2)

sumaVectores (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sumaEsIgual r f1 f2 = abs ((f1 + f2) - r) <= 0.00001 


-- Problema 1: Complete esta funcion de
-- Haskell. Esta funcion toma como parametros
-- 1. Coordenadas que indican cual es el centro
--    de un circulo.
-- 2. Un numero que indica cual es el radio del
--    circulo
-- 3. Un punto arbitrario en el plano cartesiano.
-- Esta funcion debe retornar True si el tercer
-- parametro se encuentra dentro del circulo
-- representado por los primeros dos parametros
-- o False de lo contrario.
estaAdentro :: (Float, Float) -> Float -> (Float, Float) -> Bool
estaAdentro (cx, cy) radio (x, y) = (x - cx) ^2 + (y - cy) ^2 <= radio ^2
--Funciona

-- Problema 2: Complete esta funcion de Haskell.
-- Esta funcion toma 3 parametros que representan
-- las tres aristas de un triangulo. Esta funcion
-- debe retornar True si el triangulo es equilatero
-- o False de lo contrario. Tome en cuenta la
-- presicion de los valores de tipo Float tal que
-- esta funcion funcione correctamente con tres
-- posiciones decimales de presicion.
esEquilatero :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
esEquilatero (x1, y1) (x2, y2) (x3, y3) = 
    --distanciaEuclideana (x1, y1) (x2, y2) == distanciaEuclideana (x3, y3) (x2, y2) && 
    --distanciaEuclideana (x3, y3) (x2, y2) == distanciaEuclideana (x1, y1) (x3, y3) && 
    --distanciaEuclideana (x1, y1) (x2, y2) == distanciaEuclideana (x1, y1) (x3, y3)
    abs (sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2) -  sqrt ((x3 - x2) ^ 2 + (y3 - y2) ^ 2)) <= 0.001 && 
    abs (sqrt ((x3 - x2) ^ 2 + (y3 - y2) ^ 2) -  sqrt ((x3 - x1) ^ 2 + (y3 - y1) ^ 2)) <= 0.001 && 
    abs (sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2) -  sqrt ((x3 - x1) ^ 2 + (y3 - y1) ^ 2)) <= 0.001
--Funciona


  
-- Problema 3: Una transformacion lineal en un
-- espacio de dos dimensiones se puede representar
-- multiplicando una matriz de 2x2 por un vector de
-- dos elementos (equivalente a una matriz de 2x1).
-- La funcion a continuacion toma como parametro una
-- pareja de parejas de Float que representa la matriz
-- y una pareja de Float que representa un vector. Su
-- tarea es implementar la multiplicacion de matrices para
-- estos parametros. El resultado debe ser un vector
-- de 2x1 que representa el resultado de dicha multiplicacion.
transformacionLineal :: ((Float, Float), (Float, Float)) -> (Float, Float) -> (Float, Float)
transformacionLineal ((m11, m12), (m21, m22)) (x, y) = ((m11 * x) + (m12 * y), (m21 * x) + (m22 * y))
    --(m11 * x + m12 * y, m21 * x + m22 * y)
--Funciona    

-- Problema 4:
-- En graficos de computadora, los objetos graficos
-- son modificados a travez de transformaciones lineales.
-- Una de las funciones de una tarjeta de video de hehco
-- es multiplicar matrices eficientemente. Una tranformacion
-- que se le puede hacer a un objeto grafico es rotarlo
-- por un angulo (en radianes). Eso se consigue mediante
-- una transformacion lineal con una "matriz de rotacion".
-- Utilize la funcion del problema #3 para implementar una
-- funcion que rota el punto proveido como segundo parametro
-- por el angulo del primer parametro. Puede aprender como
-- se definien las matrices de rotacion aqui: https://en.wikipedia.org/wiki/Rotation_matrix
rotacion :: Float -> (Float, Float) -> (Float, Float)
rotacion angulo punto = transformacionLineal ((m11, m12), (m21, m22)) punto

    where
    m11 = cos angulo
    m12 = -sin angulo
    m21 = sin angulo
    m22 = cos angulo
--Funciona


main = undefined