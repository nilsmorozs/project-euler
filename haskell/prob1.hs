module Main where

answer = sum [n | n <- [1..1000-1], (n `mod` 5) == 0 || (n `mod` 3) == 0]

main = print ("The answer is " ++ show answer)
