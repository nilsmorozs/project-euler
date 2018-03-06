module Main where

import NilsNumLib

-- Function 'primes' is included in "NilsNumLib.hs"
answer = head (primes 10001 0 [])

main = print ("The answer is " ++ show answer)
