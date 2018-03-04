module Main where

import NilsNumLib

answer = sum [n | n <- [1..1000-1], n `isDivisibleBy` 5 || n `isDivisibleBy` 3]

main = print ("The answer is " ++ show answer)
