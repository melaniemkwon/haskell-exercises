{- |
Module      :  disprove goldbachs other conjecture
Author      :  Melanie Kwon (CIN 303229971)

CS5035 Topics in Functional Programming, Fall 2017
Project 1. Introduction and starting out: Goldbach’s other conjecture 

-- Goldbach's other conjecture (disproved) :
   Every odd composite number can be expressed as 
   the sum of a prime and twice a square.

The program finds the first two non-prime odd numbers so that for each there is 
no prime p and integer k > 0 such that the number is equal to p + 2 * k^2.
-}

oddsFrom3 :: [Integer]
-- infinite list of odd integers
oddsFrom3 = [3, 5 .. ]

primeDivisors :: Integer -> [Integer]
-- Arguments: integer n
-- Return: integer -- square root of 'n' floored
primeDivisors n = [d | d <- takeWhile(\x -> x^2 <= n) primes, n `mod` d == 0]

primes :: [Integer]
-- infinite list of prime integers
primes = 2 : [n | n <- oddsFrom3, null (primeDivisors n)]

iSqrt :: (Integral a, Integral b) => a -> b
-- Arguments: integer n
-- Return: integer -- square root of 'n' floored
iSqrt n = floor (sqrt (fromIntegral n))

isASquare :: Integral a => a -> Bool
-- Arguments: integer n
-- Return: boolean -- True if 'n' a perfect square, otherwise False
isASquare n = (iSqrt n) ^ 2 == n 

isPrime :: Integer -> Bool
-- Arguments: integer n
-- Return: boolean -- True if 'n' is prime, otherwise False
isPrime n = n `elem` ( takeWhile(<=n) primes )

goldbachPK :: Integral t => Integer -> [(Integer, t)]
-- Arguments: odd non-prime integer g
-- Return:
-- List of integer pairs 'p' and 'k' that meet the conjecture, where
-- 'g' is the sum of a prime 'p' and twice a square k
--      p + 2 * k * k, where k > 0
goldbachPK g = [(p, iSqrt kk) | p <- takeWhile(<g) primes,
                                let kk = (g - p) `div` 2, isASquare kk]

oddComposites :: [Integer]
-- Infinite list of odd composite numbers
oddComposites = [n | n <- oddsFrom3, not (isPrime n)]

goldbachDisprove :: [Integer]
-- Disprove conjecture by getting list of numbers that lack (p,k) pairs
goldbachDisprove = [g | g <- oddComposites, null (goldbachPK g)]




