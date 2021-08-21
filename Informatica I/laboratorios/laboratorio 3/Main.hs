{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude(
    Bool,
    Float,
    undefined
    )

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
estaAdentro (cx, cy) radio (x, y) = undefined

-- Problema 2: Complete esta funcion de Haskell.
-- Esta funcion toma 3 parametros que representan
-- las tres aristas de un triangulo. Esta funcion
-- debe retornar True si el triangulo es equilatero
-- o False de lo contrario. Tome en cuenta la
-- presicion de los valores de tipo Float tal que
-- esta funcion funcione correctamente con tres
-- posiciones decimales de presicion.
esEquilatero :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
esEquilatero (x1, y1) (x2, y2) (x3, y3) = undefined

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
transformacionLineal ((m11, m12), (m21, m22)) (x, y) = undefined

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
rotacion angulo punto = undefined

main = undefined
