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





--mySnoc :: a -> [a] -> [a]
--mySnoc x xs = xs ++ [x]


--majReverse :: [a] -> [a]
--majReverse [] = []
--majReverse (x:xs) = mySnoc x (majReverse xs)


--foldek :: [a] -> [a]
--foldek = foldr mySnoc []

foldek :: [a] -> [a]
foldek = foldr mySnoc []
       where mySnoc x xs = xs ++ [x]


