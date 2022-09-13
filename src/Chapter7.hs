module Chapter7 where

--------------------------------------------------------------------------------------------------


-- 1. Show how the list comprehension [f x | x <- xs, p x] can be re-expressed
-- using the higher-order functions 'map' and 'filter'


-- [f x | x <- [1..10], even x] == map f (filter even [1..10])
-- [f x | x <- xs, p x] == map f (filter p xs)

--------------------------------------------------------------------------------------------------



-- 2. Without looking at the definitions from the standard prelude, define the
-- following higher-order library functions on lists.

-- (a) 
-- Decide if all elements of a list satisfy a predicate:
-- all :: (a -> Bool) -> [a] -> Bool

-- Different than Hutton's solution
myAll :: (a -> Bool) -> [a] -> Bool
myAll f xs = if length [x | x <- (map f xs), x == True] == length (xs) then True else False 


-- (b)
-- Decide if any element of a list satisfies a predicate:
-- any :: (a -> Bool) -> [a] -> Bool

-- Different than Hutton's solution
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = if length [x | x <- (map f xs), x == False] == length (xs) then False else True

-- (c)
-- Select elements from a list while they satisfy a predicate:
-- takeWhile :: (a -> Bool) -> [a] -> [a]

-- MY IDEA NOT WORKING
-- myTakeWhile :: (a -> Bool) -> [a] -> [a]
-- myTakeWhile f [] = [] 
-- myTakeWhile f (x:y:xs) 
--     | f x = x : (myTakeWhile f (y:xs))
--     | otherwise = []

myTakeWhile' :: (a -> Bool) -> [a] -> [a]
myTakeWhile' f [] = [] 
myTakeWhile' f [x] = if f x then (x : myTakeWhile' f []) else []
myTakeWhile' f (x:y:xs) 
    | f x = x : (myTakeWhile' f (y:xs))
    | otherwise = []

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f [] = [] 
myTakeWhile f (x:xs)
    | f x = x : (myTakeWhile f xs)
    | otherwise = []


-- (d)
-- Remove elements from a list while they satisfy a predicate:
-- dropWhile :: (a -> Bool) -> [a] -> [a]


myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile f [] = []
myDropWhile f (x:xs)
    | f x = myDropWhile f xs
    | otherwise = (x:xs)

--------------------------------------------------------------------------------------------------

{- 3.Redefine the functions map f and filter p using foldr -}

mapu :: (a -> b) -> [a] -> [b]
mapu f = foldr g []
    where g = (\x xs -> f x : xs)

filteru :: (a -> Bool) -> [a] -> [a]
filteru p = foldr g []
    where g = (\x xs -> if p x then x : xs else xs)

--------------------------------------------------------------------------------------------------


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
altMap f g [] = []
altMap f g [x] = [f x]
altMap f g (x:y:xs) = f x : g y : altMap f g xs

--------------------------------------------------------------------------------------------------


{- Exercise 0.

Using altMap , define a function luhn :: [Int] -> Bool that implements the Luhn algorithm
from the exercises in chapter 4 for bank card numbers of any length. Test your new function using
your own bank card.



Previous code from chapter4:

luhnDouble :: Int -> Int
luhnDouble x = if (2 * x) > 9 then (2 * x) - 9 else 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z g = if ( (luhnDouble z) + y  +  (luhnDouble x)  + g ) `mod` 10 == 0 then True else False

-}

luhnDouble :: Int -> Int
luhnDouble x = if (2 * x) > 9 then (2 * x) - 9 else 2 * x

-- Seems like American Express cards are not working for this, why?
-- Maybe there is a discrepancy between Original Algo and procedure
-- provided in Chapter4 by prof. Hutton?


-- American Express 	371449635398431 --> False?
-- Diners Club 30569309025904 --> True
-- Discover 6011111111111117 --> 
-- JCB 3530111333300000 --> True
-- MasterCard 5555555555554444 --> True
-- Visa 4111111111111111 --> True

luhn :: [Int] -> Bool 
luhn [] = False 
luhn (x:xs) | sum (altMap luhnDouble id (x:xs)) `mod` 10 == 0 = True 
              | otherwise = False 

--------------------------------------------------------------------------------------------------
