module Main where

import NilsNumLib

-- Define a function that factorises the number recursively using unique factors only 
-- (except when there is no choice for the last factor)
-- Function 'minFactor' is included in "NilsNumLib.hs"
factoriseUnique :: Integral a => a -> [a] -> [a]
factoriseUnique num [] = factoriseUnique num [minFactor num 2]
factoriseUnique num x
           | ((product x) == num) = x
           | ((quot num (product x)) `elem` x) = ((quot num (product x)) : x)
           | otherwise = factoriseUnique num ((minFactor (quot num (product x)) ((head x)+1)) : x)

-- Find a minimum set of factors that covers all numbers between 1 and 'factor'
findMinSetFactors :: Integral a => a -> [a] -> [a]
findMinSetFactors 1 x = x
findMinSetFactors factor [] = findMinSetFactors (factor-1) (factoriseUnique factor [])
findMinSetFactors factor x = findMinSetFactors (factor-1) (x ++ newFactors)
                               where newFactors = [f | f <- (factoriseUnique factor []), not (f `elem` x)]

answer = product (findMinSetFactors 20 [])

main = print ("The answer is " ++ show answer)
