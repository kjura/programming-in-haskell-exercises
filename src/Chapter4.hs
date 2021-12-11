module Chapter4 where

-------------------------------------------------------------------------------------
-- Exercise 1
--Using library functions, define a function halve :: [a] -> ([a],[a]) 
-- that splits an even-lengthed list into to halves. E.g in a new line:
-- halve [1, 2, 3, 4, 5, 6] -> ([1, 2, 3], [4, 5, 6])


halve :: [a] -> ([a], [a])
halve xs = ( take (length(xs) `div` 2) xs, drop (length(xs) `div` 2) xs)

-------------------------------------------------------------------------------------

-- Exercise 2
-- Define a function third :: [a] -> a that returns the third element in a list that contains at least 
-- this many elements using:
-- head and tail;
-- list indexing !!;
-- pattern matching;


-- Using head and tail 

third_ht :: [a] -> a
third_ht xs = head (tail (tail xs))


-- Using list indexing !!;

third_li :: [a] -> a
third_li xs = xs !! 2


-- Using pattern matching;

third_pm :: [a] -> a
third_pm (_:_:x:_) = x


-------------------------------------------------------------------------------------
-- Exercise 3

-- using pattern matching;

safetail_pm :: [a] -> [a]
safetail_pm [] = []
safetail_pm xs = tail (xs)

-- using guarded equations;

safetail_ge :: [a] -> [a]
safetail_ge xs | null (xs) == True = []
               | otherwise         = tail []


-- using conditional expressions

safetail_ce :: [a] -> [a]
safetail_ce xs = if null (xs) == True then [] else tail(xs)

 
-------------------------------------------------------------------------------------

-- Exercise 4
-- In a similar way to &&, show how the disjunction operator || can be defined in four
-- different ways using pattern matching.


disjun :: Bool -> Bool -> Bool
True `disjun` False = True
True `disjun` False = True
False `disjun` True = True
False `disjun` False = False


-------------------------------------------------------------------------------------

--Exercise 5
-- 

------------------------------------------------------------------------------------


-- Exercise 7
-- Show how the meaning of the following curried function definition can be formalised in terms of
-- lambda expressions:
--mult :: Int -> Int -> Int -> Int
--mult x y z = x*y*z

mult_lambda :: Int -> Int -> Int -> Int 
mult_lambda = \x -> (\y -> (\z -> x * y * z))

mult :: Int -> Int -> Int -> Int
mult x y z = x*y*z


-------------------------------------------------------------------------------------
-- Exercise 8 (Luhn algorithm for 4 digits )


luhnDouble :: Int -> Int
luhnDouble x = if (2 * x) > 9 then (2 * x) - 9 else 2 * x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z g = if ( (luhnDouble z) + y  +  (luhnDouble x)  + g ) `mod` 10 == 0 then True else False

