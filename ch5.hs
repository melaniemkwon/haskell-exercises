{-
    RECURSION
-}

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list" --if list is empty, crash
maximum' [x] = x --edge condition
maximum' (x:xs)  --use pattern matching to split a list into a head and a tail
    | x > maxTail = x  --check if head is greater than the max of rest of list, if it is- return head
    | otherwise = maxTail -- otherwise, return max of rest of list
    where maxTail = maximum' xs --define maxTail as max of rest of the list

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)

-- replicate takes Int and some element and returns a list that has several repetitions
-- of that element. ex: replicate' 3 5 returns [5,5,5]
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []  -- if we try to take 0 or neg number, we get an empty list
take' _ []      = []  -- if we try to take anything from an empty list, we get empty list
take' n (x:xs) = x : take' (n-1) xs  -- breaks list into head and tail, then state 
                                     -- that taking n elements from a list equals 
                                     -- a list that has x and the head and list that takes 
                                     -- n-1 elements from the tail as tail

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x:repeat' x 

-- take two lists and zip them together
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip xs ys

-- takes an element and a list and sees if that element is in the list
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs

-- Quicksort
-- list of items that can be sorted has type as instance of Ord typeclass
quicksort :: (Ord a) => [a] -> [a]
-- A sorted list is a list that has all the values less than (or equal to) the head
-- of the list in front (and those values are sorted), then comes the head of the
-- list in the middle and then comes all the values that are bigger than the head
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted










