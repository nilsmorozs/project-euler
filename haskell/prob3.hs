module Main where

import NilsNumLib

-- Functions 'factorise' and 'isPrime' are included in "NilsNumLib.hs"              
answer = maximum [x | x <- factorise 600851475143, isPrime x]

main = print ("The answer is " ++ show answer)
