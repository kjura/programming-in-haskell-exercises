module Chapter8 where

-----------------------------------------------------------------------------------------------------------------------


{-

8.9.1

In a similar manner to the function add, define a recursive multiplication function
mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:
Hint: make use of add in your definition.

-}


data Nat = Zero | Succ Nat
            deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n


int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ ( int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add n (mult m n)

-----------------------------------------------------------------------------------------------------------------------

{-

8.9.2

Although not included in appendix B, the standard prelude defines

data Ordering = LT | EQ | GT

together with a function:

compare :: Ord a => a -> a -> Ordering

that decides if one value in an ordered type is less than (LT), equal to (EQ), or greater than (GT)
another value. Using this function, redefine the function:

occurs :: Ord a => a -> Tree a -> Bool 

for search trees. Why is this new definition more efficient than the original version?

-}


-- Note to yourself: Just for fun, implement a function toBinTree :: [a] -> Tree a
-- for a quick conversion of a list to Binary Tree

data List a = Nil | Cons a (List a)
                deriving Show

myLen :: List a -> Int
myLen Nil = 0
myLen (Cons _ xs) = 1 + myLen xs 


data Tree a = Leaf a | Node (Tree a) a (Tree a)
                deriving Show

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))


occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x  (Node l y r) = x == y || occurs x l || occurs x r


flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r


searchTreeOccurs :: Ord a => a -> Tree a -> Bool
searchTreeOccurs x (Leaf y)                 = x == y
searchTreeOccurs x (Node l y r) | x == y    = True
                                | x < y     = searchTreeOccurs x l
                                | otherwise = searchTreeOccurs x r

-- NOT FINISHED YET!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
redefinedOccurs :: Ord a => a -> Tree a -> Bool
redefinedOccurs = undefined -- skipping for now (hint -> use `case` structure)
-----------------------------------------------------------------------------------------------------------------------


{-

8.9.3 

Consider the following type of binary trees:
data Tree a = Leaf a | Node (Tree a) (Tree a)

Let us say that such a tree is balanced if the number of leaves in the left and right subtree of every
node differs by at most one, with leaves themselves being trivially balanced. Define a function
balanced :: Tree a -> Bool that decides if a binary tree is balanced or not.

Hint: first define a function that returns the number of leaves in a tree.

-}


-- Tree with data only in its leaves

data TreeDataOnlyInLeaves a = LeafData a | NodeWithoutData (TreeDataOnlyInLeaves a) (TreeDataOnlyInLeaves a)
                            deriving Show


-- Let's try to unpack left and right side
-- thanks to geekforgeeks for the idea 
-- https://www.geeksforgeeks.org/write-a-c-program-to-get-count-of-leaf-nodes-in-a-binary-tree/
getNumberOfLeaves :: Integral a => TreeDataOnlyInLeaves a -> a
getNumberOfLeaves (LeafData n) = 1
getNumberOfLeaves (NodeWithoutData leftNode rightNode) = getNumberOfLeaves leftNode + getNumberOfLeaves rightNode
-- TODO: Just for fun, implement a function that converts [Int] -> TreeDataOnlyInLeaves

balanced :: Integral a => TreeDataOnlyInLeaves a -> Bool
balanced (LeafData a) = True
balanced (NodeWithoutData left_node right_node) = abs (getNumberOfLeaves left_node - getNumberOfLeaves right_node) <= 1 && balanced left_node && balanced right_node


-- NodeWithoutData (NodeWithoutData (LeafData 6) (LeafData 9)) (NodeWithoutData (LeafData 4) (LeafData 2))
-- getNumberOfLeaves (LeafData 3)
-- getNumberOfLeaves (NodeWithoutData (LeafData 6) (LeafData 9))
-- NodeWithoutData (NodeWithoutData (NodeWithoutData (LeafData 6) ))
-- myCuteTree = 
-- NodeWithoutData (NodeWithoutData (LeafData 6) (NodeWithoutData (LeafData 7) (LeafData 9))) (LeafData 3) 


-----------------------------------------------------------------------------------------------------------------------

{-

8.9.4

Define a function balance :: [a] -> Tree a that converts a non-empty list into a balanced tree.
Hint: first define a function that splits a list into two halves whose length differs by at most one.


-}

-- We cut off everything after a dot, e.g 2.5 --> 2, 3.5 --> 3, taking only the integer part
splits :: [a] -> ([a], [a])
splits [] = ([], [])
splits xs = splitAt (div (length xs) 2) xs

balanceTree :: [a] -> TreeDataOnlyInLeaves a
balanceTree [] = error "The list cannot be empty!"
balanceTree [e] = LeafData e -- Trivially balanced Leaf e
balanceTree xs =  NodeWithoutData (balanceTree $ fst splitted) (balanceTree $ snd splitted) -- continuously split list and apply 
    where
        splitted = splits xs

-----------------------------------------------------------------------------------------------------------------------

{-

8.9.5

Given the type declaration
data Expr = Val Int | Add Expr Expr

define a higher-order function folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
such that folde f g replaces each Val constructor in an expression by the function f, and each
Add constructor by the function g.

-}

data Expr = Val Int | Add Expr Expr
    deriving Show

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val m) = f m
folde f g (Add exprA exprB) = g (folde f g exprA) (folde f g exprB)
