module Main where

import NilsNumLib

-- Function 'isPalindrome' is included in "NilsNumLib.hs"
answer = maximum [x*y | x <- [100..999], y <- [100..999], isPalindrome (x*y)]

main = print ("The answer is " ++ show answer)
