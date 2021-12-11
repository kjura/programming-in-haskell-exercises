module Chapter6 where

-- Exercise 1
-- How does the recursive version of the factorial function behave if applied to a negative
-- argument, such as (-1)? Modify the definition to prohibit negative arguments
-- by adding a guard to the recursive case.


-- It will quickly start reaching infinite case, the base case will never be met
-- as myFac(n - 1) will start reaching negative values going for minus infinity

myFac :: Int -> Int
myFac 0 = 1
myFac n | n > 0 = n * myFac(n - 1)
        | otherwise = 1


 


-- Exercise 2
-- Define a recursive function sumdown :: Int -> Int that returns the sum of the non-negative
-- integers from a given value down to zero. For example, sumdown 3 should return the result
-- 3 + 2 + 1 + 0 = 6


sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown(n - 1)



-- Exercise 3
-- Define the exponentiation operator ^ for non-negative integers using the same pattern
-- of recursion as the multiplication operator *, and show how the expression 2 ^ 3 is
-- evaluated using your definition


myExpo :: Int -> Int -> Int
myExpo m 0 = 1
myExpo m n = m * (myExpo m (n - 1))


-- Exercise 4
-- Define a recursive function euclid :: Int -> Int -> Int that implements Euclid's algorith
-- for calculating the greatest common divisor of two non-negative integers: if the two numbers
-- are equal, this number is the result; otherwise, the smaller number is subtracted from
-- the larger, and the same process is then repeated. E.g euclid 6 27 -----> 3

euclid :: Int -> Int -> Int
euclid a b | a > b = euclid (a - b) b
           | b > a = euclid a (b - a)
           | a == b = a



-- Exercise 6 Without looking at the definition from the standard prelude, define the following
-- library functions on lists using recursion

-- a.
-- Decide if all logical values in a list are True
-- myAnd :: [Bool] -> Bool


myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd [False] = False
myAnd [True] = True
myAnd (x:xs) | tail(xs) == [] = head(xs)
             | otherwise = myAnd (head(xs):tail(xs))


-- b.
-- Concatenate a list of lists
-- myConcat :: [[a]] -> [a]

myConcat :: [[a]] -> [a]
myConcat [[]] = []
myConcat [x:xs] = x:(myConcat [xs])

-- c.
-- Produce a list with n identical elements:
-- myReplicate :: Int -> a -> [a]

myReplicate :: Int -> a -> [a]
myReplicate 0 x = []
myReplicate n x = x : (myReplicate (n - 1) x)


-- d.
-- Select the nth element of a list
-- (!!) :: [a] -> Int -> a


mySelect :: [a] -> Int -> a
mySelect (x:xs) 0 = x
mySelect (x:xs) n = mySelect xs (n-1)

-- e.
-- Decide if a value is an element of a list
-- elem :: Eq a => a -> [a] -> Bool 



myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) | a == x = True
                | otherwise = elem a xs


-- Exercise 7
-- Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges
-- two sorted lists to give a single sorted list. For example:
-- merge [2, 5, 6] [1, 3, 4]
-- [1, 2, 3, 4, 5, 6]


myMerge :: Ord a => [a] -> [a] -> [a]
myMerge [] [] = []
myMerge (x:xs) [] = x:xs
myMerge [] (y:ys) = y:ys




-- Exercise 9
-- Using the five-step process, construct the library functions that:

-- a. calculate the sum of a list of numbers;


fiveSum :: Num a => [a] -> a
fiveSum [] = 0
fiveSum (x:xs) = x + (fiveSum xs) 


-- b. take a given number of elements from the start of a list.


myTake :: Int -> [a] -> [a]
myTake 0 (x:xs) = []
myTake n [x] = [x]
myTake n [] = []
myTake n (x:xs) = [x] ++ myTake (n-1) xs


-- c. select the last element of a non-empty list.


myLast :: [a] -> a
myLast [] = error "Non-empty list not allowed"
myLast [x] = x
myLast (x:xs) = head (myLast xs : x : [])

















