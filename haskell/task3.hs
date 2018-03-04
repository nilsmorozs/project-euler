module Main where

import NilsNumLib
              
answer = maximum [x | x <- factorise 600851475143 [], isPrime x]

main = print ("The answer is " ++ show answer)
