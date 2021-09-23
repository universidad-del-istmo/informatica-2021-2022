module Main where

import Prelude (Show, undefined)

data Natural = O | Succ Natural deriving Show

predecesor O = O
predecesor (Succ n) = n

main = undefined 
