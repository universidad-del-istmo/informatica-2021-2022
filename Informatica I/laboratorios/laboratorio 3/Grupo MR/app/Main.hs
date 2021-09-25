{-#LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (undefined, Int, Float, (/=), (/), cos, sin, acos, asin, atan, sqrt, abs, pi, (<), (<=), (>), (>=), (^), (+), div, mod, (-), (*), (==), Bool(..), (&&), (||))


-- PROBLEMA 1: Complete esta funcion de
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

unBool :: Bool
unBool = True

otroBool :: Bool
otroBool= False 

--esCirculo (cx, cy) (r) (px, py) = (px-cx)^2 + (py-cy)^2 == r^2
estaAdentro (cx, cy) (r) (px, py) = (px-cx)^2 + (py-cy)^2 <= r^2 

-- PROBLEMA 2: Complete esta funcion de Haskell.
-- Esta funcion toma 3 parametros que representan
-- las tres aristas de un triangulo. Esta funcion
-- debe retornar True si el triangulo es equilatero
-- o False de lo contrario. Tome en cuenta la
-- presicion de los valores de tipo Float tal que
-- esta funcion funcione correctamente con tres
-- posiciones decimales de presicion.
esEquilatero :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool

d1 (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)
d2 (x2, y2) (x3, y3) = sqrt ((x3 - x2)^2 + (y3 - y2)^2)
d3 (x3, y3) (x1, y1) = sqrt ((x1 - x3)^2 + (y1 - y3)^2)

ang1 (x1, y1) (x2, y2) (x3, y3)  = acos ((d3 (x3, y3) (x1, y1) / 2 ) /  d1 (x1, y1) (x2, y2))
ang2 (x1, y1) (x2, y2) (x3, y3) = acos ((d3 (x3, y3) (x1, y1) / 2 ) /  d2 (x2, y2) (x3, y3))
ang3 (x1, y1) (x2, y2) (x3, y3) = 2 * (asin (((d3 (x3, y3) (x1, y1) / 2 ) ) /d2 (x2, y2) (x3, y3)))

--esEquilatero (x1, y1) (x2, y2) (x3, y3) = 
    --ang1 (x1, y1) (x2, y2) (x3, y3) - ang2 (x1, y1) (x2, y2) (x3, y3) <= 0.01 &&
    --ang2 (x1, y1) (x2, y2) (x3, y3) - ang3 (x1, y1) (x2, y2) (x3, y3) <= 0.01 &&
    --ang3 (x1, y1) (x2, y2) (x3, y3) - ang1 (x1, y1) (x2, y2) (x3, y3) <= 0.001

--esEquilatero (x1, y1) (x2, y2) (x3, y3) =  
   -- (ang1 (x1, y1) (x2, y2) (x3, y3) +  ang2 (x1, y1) (x2, y2) (x3, y3) + ang3 (x1, y1) (x2, y2) (x3, y3)) - (pi) <= 0.0001 

esEquilatero (x1, y1) (x2, y2) (x3, y3) = 
     (d1 (x1, y1) (x2, y2) - d2 (x2, y2) (x3, y3)) <= 0.001 &&
     (d2 (x2, y2) (x3, y3) - d3 (x3, y3) (x1, y1)) <= 0.001 &&
     (d3 (x3, y3) (x1, y1) - d1 (x1, y1) (x2, y2)) <= 0.001 
 

-- PROBLEMA #3: Una transformacion lineal en un
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

transformacionLineal ((m11, m12), (m21, m22)) (x, y)= 
    (((m11 * x) + (m12*y)), ((m21*x) + (m22*y)))


-- PROBLEMA #4: En graficos de computadora, los objetos graficos
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

rotacion angulo (x,y) = 
    (((x * cos (angulo)) - (y * sin (angulo))), 
    ((x * sin (angulo)) + (y * cos (angulo ))))
    


main = undefined
