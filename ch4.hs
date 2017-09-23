lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"

-- Without pattern matching
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  

-- Define factorial recursively.
-- Start by saying that factorial of 0 is 1, then state that factorial of any
-- positive integer is that integer multiplied by the factorial of its predecessor.
factorial :: (Integral a) => a -> a 
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"
-- Always include some kind of catch-all or you'll get errors
charName x = "Scooby dooby doo"

-- Pattern matching can also be used in tuples
-- function that adds two vectors
-- without pattern matching
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)
-- with pattern matching
addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first(x, _, _) = x
second :: (a, b, c) -> b 
second (_, y, _) = y
-- make our own triples function
third :: (a, b, c) -> c
third(_, _, z) = z
-- NOTE: The _ means the same thing as it does in list comprehensions. 
-- It means that we really don't care what that part is, so we just write a _.

-- Pattern match list comprehensions
-- ghci commands
-- let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
-- [a+b | (a,b) <- xs]  
samelist1 = [1,2,3]
samelist2 = 1:2:3:[]
-- NOTE: a pattern like x:xs will bind the head to x and rest to xs,
-- even if there's only one element so xs ends up being an empty list
-- NOTE2: patterns with : only match against lists of length 1 or more

-- own implementation of head function, pattern matching on list
head' :: [a] -> a 
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
-- CASE EXPRESSION version of head function
-- case expression of pattern -> result
--                    pattern -> result
head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"
                       (x:_) -> x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are " ++ show x ++ " and " ++ show y

-- length function using pattern matching and recursion
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- PATTERN - breaking something up according to a pattern and beinging it to names
-- while keeping a reference to the whole thing.
capital :: String -> String
capital "" = "Empty string, woops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards test whether some property of value are true or false
-- Guard are indicated by pipes following a function's name and its parameters
-- it's basically a bool expression. If it evaluates to True, then correspondign
-- function body is used
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          (skinny, normal, fat) = (18.5, 25.0, 30.0)  

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT 
    | a == b    = EQ
    | otherwise = LT 

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- can put let bindings inside list comprehensions
-- include let like a predicate, but it doesn't filter the list, it only binds to names
-- the names defined in let are visible to the output function (part before |)
calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h^2]
-- bmis of fatties only
calcBmisFatties :: (RealFloat a) => [(a, a)] -> [a]
calcBmisFatties xs = [bmi | (w, h) <- xs, let bmi = w / h^2, bmi >= 25.0]

-- 'let' bindings let you bind to variables anywhere and are local expressions
-- let <bindings> in <expression>
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r^2
    in sideArea + 2 * topArea

-- Whereas pattern matching on function parameters can only be done when defining
-- functions, case expressions can be used pretty much anywhere.
describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

-- can also be defined as
describeList :: [a] -> String
describeList xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."











