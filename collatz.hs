-- odd multiply by 3 and add 1
-- even divide by half

-- recursive function
collatz 1 = [1]  --this clause only runs if argument is 1. if it is triggered, it returns a list returning 1

collatz n -- two possibilities, can also do with if then else.  Can also write otherwise = n:...
    | even n =  n:collatz (n `div` 2)  -- if the number is even, then take collatz sequence, n div 2. And take number you've got and put it in front.
    | odd n  =  n:collatz (n*3 + 1)    -- if the number is odd,
    -- | otherwise  =  n:collatz (n*3 + 1) 

-- Also can write as...
collatz n  = n : (if even n then collatz (n `div` 2)
                            else collatz (n*3 + 1)
                 )

-- Also as...
collatz n  = n:collatz(if even n then n `div` 2
                            else n*3 + 1
                        )

---------------------------------------------------------
-- QuickSort is a Divide and Conquer algorithm. It picks an element as pivot 
-- and partitions the given array around the picked pivot.
quicksort [] = []  
quicksort (x:xs) =   -- first element x, and rest of elements xs
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

-- http://zvon.org/other/haskell/Outputsyntax/letQexpressions_reference.html






