module Main where

import NilsNumLib

-- Function that finds the lowest triangle number value with 'minNumDiv' divisors
findNumber :: Integral a => a -> a -> Int -> a
findNumber num index minNumDiv = if numDiv >= minNumDiv
                                 then num
                                 else findNumber (num+index+1) (index+1) minNumDiv
                                   where numDiv = length $ findFactors num []
                                   
-- Partial application of the above function for readability (tell it that first triangle number is one)
findNumWithDiv = findNumber 1 1
                 
-- The answer is the triangle number with at least 501 divisors (1st triangle number is one)
answer = findNumWithDiv 501

-- Main function that prints the answer
main = print ("The answer is " ++ show answer)
