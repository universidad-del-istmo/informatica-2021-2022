---------------------------------------
-- laboratorio_6.hs
---------------------------------------
-- UNIVERSIDAD DEL ISTMO
-- Informática I
-- Max Marroquín, Juan Pablo Zebadúa
-- Laboratorio 6
---------------------------------------

-- Problema #1
minMax xs = (minimum(foldl (++) [] [xs]), maximum(foldl (++) [] [xs]))

-- Problema #2
-- Criterios: "compararInts" "compararIntsInv"
minMaxBy xs p
    |p == "compararInts" = (minimum(foldl (++) [] [xs]), maximum(foldl (++) [] [xs]))
    |p == "compararIntsInv" = (maximum(foldl (++) [] [xs]), minimum(foldl (++) [] [xs]))
    |otherwise = (0,0)

-- Problema #3
fromDigits [] = 0
fromDigits (x:xs) = foldl (*) (10^(length(x:xs)-1)) [x] + fromDigits xs