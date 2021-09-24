{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude(abs, sqrt, (<), (<=), (/), (^), (+), (-), div, mod, (*), (==), (/=), (>), (>=), (&&), (||), pi, sin, cos,
    Bool,
    Float,
    undefined
    )

--Jose Humberto Najar Venavente 00000013661

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

estaAdentro (cx, cy) radio (x, y) =
    
       sqrt ((cx - x) ^ 2 + (cy - y) ^2) <= radio ^2

-- Problema 2: Complete esta funcion de Haskell.
-- Esta funcion toma 3 parametros que representan
-- las tres aristas de un triangulo. Esta funcion

-- debe retornar True si el triangulo es equilatero
-- o False de lo contrario. Tome en cuenta la
-- presicion de los valores de tipo Float tal que
-- esta funcion funcione correctamente con tres
-- posiciones decimales de presicion.

esEquilatero :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool

esEquilatero (x1, y1) (x2, y2) (x3, y3) = (ladodeabajo + ladoizquierdo) - (ladoderecho + ladoizquierdo) <= 0.01

    where
        ladodeabajo =    sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^2)
        ladoizquierdo =  sqrt ((x1 - x3) ^ 2 + (y1 - y3) ^2)
        ladoderecho =    sqrt ((x3 - x2) ^ 2 + (y3 - y2) ^2)

    --                x3
    --
    --
    --
    --
    --       x1                x2

    -- distanciaEuclideana (x1, y1) (x2, y2) == distanciaEuclideana (x2, y1) (x2, y1) &&
    -- distanciaEuclideana (x3, y3) (x1, y1) == distanciaEuclideana (x3, y3) (x1, y1) &&
    -- distanciaEuclideana (x2, y2) (x3, y3) == distanciaEuclideana (x2, y2) (x3, y3) &&


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
transformacionLineal ((m11, m12), (m21, m22)) (x, y) =  (matriz1, matriz2)

--(m11  * x + m12 * y, m21 * x + m22 * y )

    where
        matriz1 = m11 * x + m12 * y
        matriz2 = m21 * x + m22 * y

    

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
operaciondeAngulo a = a * 2 * pi / 360

rotacion :: Float -> (Float, Float) -> (Float, Float)
rotacion angulo = transformacionLineal ((coseno, senoNegativo), (seno,cosenoSda))
    where
        resultadodeAngulo = operaciondeAngulo angulo
        coseno            = cos resultadodeAngulo
        senoNegativo      = -1 * sin resultadodeAngulo
        seno              = sin resultadodeAngulo
        cosenoSda         = cos resultadodeAngulo


main = undefined


