module NilsNumLib where

import Data.List

-- Function that checks if n is disible by m
isDivisibleBy n m = if ((mod n m) == 0) then True else False 

-- Function that finds the smallest factor in a number recursively, starting search from 'n'
minFactor :: Integral a => a -> a -> a
minFactor num n 
     | (n*n > num) = num  
     | otherwise = if num `isDivisibleBy` n
                   then n
                   else minFactor num (n+1)
                           
-- Function that factorises the number
factorise :: Integral a => a -> [a]
factorise 1 = []
factorise 2 = [2]
factorise num = factor : factorise (num `quot` factor)
                   where factor = minFactor num 2
                           
-- Function that finds all unique factors of the number recursively starting from a set 'x' (leave empty [])
findFactors :: Integral a => a -> [a] -> [a]
findFactors num [] = findFactors num [1]
findFactors num x  = if (head x) == num
                           then x
                           else findFactors num $ (minFactor num ((head x)+1)) : x
              
-- Function that generates primes up to a certain number using simple trial division
primesUpTo :: Integral a => a -> [a]
primesUpTo maxNum = 2 : filter isPrime [3, 5.. maxNum]
             
-- Function that checks if the input number is a prime
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime num = (minFactor num 2) == num

-- Function that finds the next prime after the number 'num'
nextPrime :: Integral a => a -> a
nextPrime num = if isPrime (num+1)
                then num+1
                else nextPrime (num+1)
             
-- Function that creates a list with a specified number of primes (quite slow, should be a better way)
primes :: Integral a => Int -> [a] -> [a]
primes numPrimes [] = primes numPrimes [2]
primes numPrimes x
             | (length x) == numPrimes = x
             | otherwise = primes numPrimes $ nextPrime (head x) : x

-- Function that generates Fibonacci numbers up to a maximum number
fibonacci :: (Integral a, Ord a) => [a] -> a -> [a]
fibonacci [] maxNum = fibonacci [2, 1] maxNum
fibonacci x maxNum
    | newNum <= maxNum = fibonacci (newNum:x) maxNum
    | otherwise = x
    where newNum = sum (take 2 x) 
    
-- Function that checks if the number is a palindrome
isPalindrome :: (Integral a, Show a) => a -> Bool
isPalindrome num = (numString == (reverse numString))
                 where numString = (show num)

-- Function that splits a number into a list of digits
digits :: Integral a => a -> [a]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]
