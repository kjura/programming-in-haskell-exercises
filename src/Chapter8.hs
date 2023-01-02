module Chapter8 where

data Nat = Zero | Succ Nat
            deriving Show

{-

8.9.1

In a similar manner to the function add, define a recursive multiplication function
mult :: Nat -> Nat -> Nat for the recursive type of natural numbers:
Hint: make use of add in your definition.

-}

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