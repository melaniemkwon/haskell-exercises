{- |
Module      :  Luhn Algorithm (AKA "mod 10")
Author      :  Melanie Kwon

CS5035 Topics in Functional Programming, Fall 2017
Project 2 - Part 1

-- Luhn Algorithm (AKA "mod 10") :
   A checksum formula used to validate identification numbers to prevent
   simple typos (i.e. credit card numbers).

Steps:
    1. Starting from the right, move left to double the value of every second 
    digit. If the doubled result is >9 then add the digits of the product.
        Ex:
        [1,  3,  8,  6] becomes..
        [2,  3,  16, 6]

    2. Take the sum of all the digits.
        Ex:
        [2,  3,  16, 6] becomes..
        2 + 3 + 1+6 + 6 = 18.

    3. Divide the sum by 10. If the remainder is 0, then the number is valid. 
    Otherwise, it's not valid.
        Ex:
        18 % 10   ->   8 (not valid)

Example test cases:
    validate 4012888888881881 = True
    validate 4012888888881882 = False
-}

-- Arguments: 
--   Integer n
-- Return: 
--   If input is positive integer, return list of positive integers, 
--       representing the digits.
--   If input is 0 or negative, return empty list.
toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

-- Arguments: 
--   Integer n
-- Return: 
--   If input is positive integer, return REVERSE order list of positive  
--       integers, representing the digits
--   If input is 0 or negative, return empty list.
-- toDigitsRev :: Integer -> [Integer]
-- toDigitsRev = reverse . toDigits

-- Arguments: 
--   List of integers
-- Return:
--   List of integers with every other number (starting from right) doubled.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . zipWith (*) oneTwo . reverse
  where oneTwo = 1 : 2 : oneTwo

-- Arguments: 
--   List of integers
-- Return: 
--   Integer sum of all digits   
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- Arguments: 
--   Integer n
-- Return: 
--   True if number is valid, otherwise false
validate :: Integer -> Bool
validate n = 
    -- sumDigits (doubleEveryOther (toDigits n)) `mod` 10 == 0
    (sumDigits . doubleEveryOther . toDigits $ n) `mod` 10 == 0

-- validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits