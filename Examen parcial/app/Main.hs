{-#LANGUAGE NoImplicitPrelude #-}
-- DIEGO GIRON Y JORGE FERGUSON --
module Main where

import Prelude (Show, undefined, putStrLn)



data Natural = O | Succ Natural deriving Show



uno = succ (O)

dos = succ ( uno)

tres = succ (dos)

cuatro = succ ( tres)

cinco = succ (cuatro)



predecesor O = O

predecesor succ ( O ) = O

predecesor succ ( succ ( O ) ) = succ ( O )



predecesor succ ( a ) = a

predecesor succ ( succ ( a ) ) = succ ( a )

predecesor succ ( succ ( succ ( a ))) = succ ( succ ( a ))


main :: IO ()
main = putStrLn "Hello, Haskell!"
