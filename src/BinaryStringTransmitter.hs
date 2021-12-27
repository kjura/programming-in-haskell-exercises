module BinaryStringTransmitter where

import Data.Char

type Bit = Int

{- Function responsible for base conversion, that is, 
converting binary number to integer written in the decimal system
Convention: Binary numbers are written in reverse order to normal
Example: 1101 would be written as 1011
-}
bit2intother :: [Bit] -> Int
bit2intother bits = sum [w * b | (w, b) <- zip weights bits]
                where weights = iterate (*2) 1


{- This is re-written below using foldr higher order function
bit2int :: [Bit] -> Int
bit2int [] = 0
bit2int (x:xs) = x + 2 * bit2int xs
-}

bit2int :: [Bit] -> Int
bit2int bits = foldr (\x xs -> x + 2 * xs) 0



