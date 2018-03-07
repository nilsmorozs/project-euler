module Main where

-- Find the product of a Pythagorean triplet that matches the criteria using list comprehension
tripletProduct = [a * b * (sqrt (a^2+b^2)) | a <- [1..1000], b <- [1..1000], a < b, a + b + (sqrt (a^2+b^2)) == 1000]

-- "round" is used for converting from Float to Int, "head" is used to convert from [a] to a
answer = round (head tripletProduct)

main = print ("The answer is " ++ show answer)
