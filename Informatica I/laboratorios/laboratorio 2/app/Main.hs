{-# LANGUAGE NoImplicitPrelude #-}

module Main where

    import Prelude (Show, UNDEFINIED)

    data Natural = O | Succ Natural deriving Show

O + m = m  
n + O = n 
n + (Succ a) = Succ (n + a)


-- factorial n = n * factorial (n- Succ O)


sonIguales O O = Succ O 
sonIguales O m = O 
sonIguales n 0 = O
sonIguales (Succ a) (Succ b) = sonIguales a b

-- sonIguales (Succ (Succ O)) (Succ (Succ O))
-- | a = Succ O, b= Succ O
-- = sonIguales (Succ O) 
-- | a = O, b= O 
-- = sonIguales O O
-- = Succ O

    uno = Succ O

    dos = Succ uno
    
    tres = Succ tres

    cuatro = Succ tres  

    main = UNDEFINIED


fib O = O 
fib (Succ O) = Succ O 
fib (Succ (Succ a)) = fib (Succ a) + fib (a)

main = UNDEFINIED





