module Chapter7 where


-- 1. Show how the list comprehension [f x | x <- xs, p x] can be re-expressed
-- using the higher-order functions 'map' and 'filter'


-- [f x | x <- [1..10], even x] == map f (filter even [1..10])
-- [f x | x <- xs, p x] == map f (filter p xs)




-- 2. Without looking at the definitions from the standard prelude, define the
-- following higher-order library functions on lists.

-- a. Decide if all elements of a list satisfy a predicate:
-- all :: (a -> Bool) -> [Bool] -> Bool
-- e.g all even [2, 4, 6, 8] ----> True




{- 4.Using foldl , define a function dec2int:: [Int] -> Int that converts a decimal number into
an integer. For example:
> newDec2int [2,3,4,5]
2345
 -}

newDec2Int :: [Int] -> Int 
newDec2Int = foldl (\x y -> 10 * x + y) 0

--------------------------------------------------------------------------------------------------

{- 
Exercise 9.

Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately applies
its two argument functions to successive elements in a list, in turn about order. For example:

altMap (+10) (+100) [0,1,2,3,4]

Output -> [10,101,12,103,14]

 -}


altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap 