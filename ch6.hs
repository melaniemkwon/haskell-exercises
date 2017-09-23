{-
    HIGHER ORDER FUNCTIONS
-}

-- Curried functions
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- create a function that takes a number and compares it to 100
compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

-- infix functions can also be partially applied by using sections
divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- checks if a char supplied is an uppercase letter
isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- function that takes a function then applies it twice to something
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- takes function has two lists as parameters then joins the lists by applying the function b/w corresponding elements
zipWidth' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWidth' _ [] _ = []
zipWidth' _ _ [] = []
zipWidth' f (x:xs) (y:ys) = f x y : zipWidth' f xs ys

-- takes a function and returns a function that's like our original function but 
-- first two arguments are flipped
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
    where g x y = f y x

-- functions are curried, so we can take advantage of currying when making higher0orer functions
-- by thinking ahead and writing what their end result would be if they were fully applied
flip'' :: (a -> b -> c) -> (b -> a -> c)
flip'' f y x = f x y

-- rewritten with lambda function
flip''' :: (a -> b -> c) -> (b -> a -> c)
flip''' f = \x y -> f y x

-- map takes a function and list and applies that function to every element int he list, 
-- producing a new list
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

-- map from foldr
map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- function that takes predicate and a list then returns the list of elements taht satisfy it
-- simply, if p w evaluates to true, it gets put into the list
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

-- find the largest number under 100,000 that's divisible by 3829
-- evaluation stops when first adequate solution from infinite list is found
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

-- collatz sequences
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

-- lambdas are anonymous functions that are used b/c we need some functions only once
numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- A fold takes a binary function, a starting value (accumulator), and a list to fold up
-- binary function itself takes 2 params--it's called with the accumulator and 
-- the first element and produces a new accumulator
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs -- foldl is left fold. it folds the list up from the left side

sum'' ::(Num a) => [a] -> a
sum'' = foldl (+) 0

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- Using foldl1 and foldr1 (which you don't need to provide starting values)
-- They assume that first (or last) element to be the starting value. Error on empty lists though.
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- find sum of all squares that are smaller than 10,000
oddSquareSum :: Integer
oddSquareSum = -- sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
               -- rewrite with function composition
               -- sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
               -- but better to write it as below so easier to read
                let oddSquares = filter odd $ map (^2) [1..]
                    belowLimit = takeWhile (<10000) oddSquares
                in sum belowLimit 








