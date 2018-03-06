module Main where

import NilsNumLib

-- Function 'fibonacci' included in "NilsNumLib.hs"
answer = sum [x | x <- (fibonacci [] 4000000), even x]

main = print ("The answer is " ++ show answer)
