{-#LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Main where

import Prelude (undefined, Bool (True, False), Float, Int, (-), (+), (^), (>=), (<=), (<), (>),
 (/),
 (==),
 div,
 sqrt,
 mod,
 (*),
 sin,
 cos,
 pi,
 Bool (..), (&&), (||)
 )
import Data.Bool (bool)
import GHC.Unicode (isPunctuation)



-- Problema 1-- 
-- 1. Coordenadas que indican cual es el centro de un circulo --
-- 2. un numero que indica cual es el radio del circulo --
-- 3. Un punto arbitrario en el plano cartesiano. --
-- Esta funcion debe retornar True si el tercer parametro se encuentra dentro del circulo representado por los primeros dos parametros --
-- O False de lo contrario --
estaAdentro :: (Float, Float) -> Float -> (Float, Float) -> Bool
estaAdentro (cx, cy) radio (x, y) = (x - cx)^2 + (y - cy)^2 <= radio



unBool :: Bool
unBool = True

otroBool :: Bool
otroBool = False


-- Problema 2 
-- complete esta funcion en haskell
-- esta funcion tomo 3 parametros que representan
-- las tres arists de un triangulo. Esta funcion
-- debe retornar true si el triangulo es equilatero
-- o False de lo contrario. Toma en cuenta que la
-- presicion de los valores de tipo Float tal que
-- esta funcion funcione correctamente con tres
-- posiiciones decimales de presicion
-- formula distancia == distancia (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y2)^2 )



esEquilatero :: (Float, Float ) -> (Float, Float ) -> (Float, Float ) -> Bool
esEquilatero (x1, y1) (x2, y2) (x3, y3) =
    distanciaAristaTrinagulo (x1, y1) (x2, y2) == distanciaAristaTrinagulo2 (x1, y1) (x3, y3)
    && distanciaAristaTrinagulo2 (x1, y1) (x3, y3) == distanciaAristaTrinagulo3 (x2, y2) (x3, y3)
    && distanciaAristaTrinagulo3 (x2,y2) (x3, y3) == distanciaAristaTrinagulo (x1, y1) (x2,y2)



distanciaAristaTrinagulo (x1, y1) (x2, y2) = sqrt ((x2-x1)^2 + (y2-y1)^2 )

distanciaAristaTrinagulo2 (x1, y1) (x3, y3) = sqrt ((x3-x1)^2 + (y3-y1)^2 )

distanciaAristaTrinagulo3 (x2, y2) (x3, y3) = sqrt ((x3 - x2 )^2 + (y3 - y2)^2)


-- Problema 3 
-- Una transformacion lineal en un
-- espacio de dos dimensiones se peude representar 
-- multiplicando una matriz de 2x2 por un vector
-- de dos elemtos (equivalente a una matriz 2x1 )
-- La funcion a continuacion toma como parametro una pareja de parejas de Float 
-- que representa la raiz la matriz y una pareja Float que representa un vector. Su tarea es implementar la multiplicacion de matrices 
-- para estos parametros. El resultado debe sr un vector de 2x1 que representa el resultado de dicah multiplicacion.
transformacionLineal :: ((Float , Float ), (Float, Float )) -> (Float, Float ) -> (Float, Float )
transformacionLineal ((m11, m12), (m21, m22)) (x,y) = ( (m11 * x) + (m12 * y), (m21 * x) + (m22 * y) )

operacionMatriz1 (m11, m12) (x,y) =
     (m11 * x) + (m12 * y)

operacionMatriz2 (m21, m22) (x,y) =
    (m21 * x) + (m22 * y)

-- Problema 4 
--En graficos de computadora, los objetos graficos 
-- son modificaciones a traves de transformaciones lineales.
-- Una de las funciones de una tarjeta de video de hecho
-- es multiplicar matrices eficientemente. Una transformacion 
-- que se le puede hacer a un objeto grafico es rotarlo
-- por un angulo (EN RADIANES). Eso se consigue mediante una transformacion lineal con una "matriz de rotacion "
-- Utilize la funcion del problema 3 para implementar una 
-- funcion que rota el punto proveido como segundo parametro
-- por el angulo del primer parametro. Puede aprender se definen las matrices de rotacion aqui:
-- https://en.wikipedia.org/wiki/Rotation_matrix


rotacion :: Float -> (Float, Float ) -> (Float, Float )

rotacion angulo (x,y) =
    ((cos angulo * x) - (sin angulo * y), (sin angulo * x) + (cos angulo * y))


main = undefined
