module NilsNumLib where

-- Define function that checks if n is disible by m
isDivisibleBy n m = if ((mod n m) == 0) then True else False 

-- Define a function that finds the smallest factor in a number recursively, starting search from 'n'
minFactor :: Integral a => a -> a -> a
minFactor num n =  if num `isDivisibleBy` n
                            then n
                            else minFactor num (n+1)
                                     
-- Define a function that factorises the number recursively starting from a set 'x' (leave empty [])
factorise :: Integral a => a -> [a] -> [a]
factorise num [] = factorise num [minFactor num 2]
factorise num x  = if (product x) == num
                           then x
                           else factorise num ((minFactor (quot num (product x)) (head x)) : x)
                           
-- Define a function that finds all unique factors of the number recursively starting from a set 'x' (leave empty [])
findFactors :: Integral a => a -> [a] -> [a]
findFactors num [] = findFactors num [1]
findFactors num x  = if (head x) == num
                           then x
                           else findFactors num ((minFactor num ((head x)+1)) : x)
              
-- Define function that generates primes up to a certain number using the Sieve of Eratosthenes
primesUpTo :: Integral a => a -> [a]
primesUpTo maxNum = sieve [2..maxNum]
             where
             sieve (x:xs) = x : sieve [sxs | sxs <- xs, not (elem sxs [x, x+x..maxNum])]
             sieve []     = []
             
-- Define function that checks if the input number is a prime
isPrime :: Integral a => a -> Bool
isPrime 1 = False
isPrime num = (minFactor num 2) == num

-- Define function that generates Fibonacci numbers up to a maximum number
fibonacci :: (Integral a, Ord a) => [a] -> a -> [a]
fibonacci [] maxNum = fibonacci [2, 1] maxNum
fibonacci x maxNum
    | newNum <= maxNum = fibonacci (newNum:x) maxNum
    | otherwise = x
    where newNum = sum (take 2 x) 
    
-- Define function that checks if the number is a palindrome
isPalindrome :: (Integral a, Show a) => a -> Bool
isPalindrome num = (numString == (reverse numString))
                 where numString = (show num)
