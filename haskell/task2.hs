module Main where

import NilsNumLib

answer = sum [x | x <- (fibonacci [] 4000000), even x]

main = print ("The answer is " ++ show answer)
