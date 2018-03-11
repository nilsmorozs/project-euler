module Main where

import NilsNumLib
      
-- Function 'primesUpTo' is defined in NilsNumLib.hs
answer = sum $ primesUpTo 2000000

-- Main function that prints the answer
main = print ("The answer is " ++ show answer)
