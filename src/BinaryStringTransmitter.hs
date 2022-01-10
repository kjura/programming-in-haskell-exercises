module BinaryStringTransmitter where

import Data.Char

type Bit = Int

{- Function responsible for base conversion, that is, 
converting binary number to integer written in the decimal system
Convention: Binary numbers are written in reverse order to normal
Example: 1101 would be written as 1011

bit2int :: [Bit] -> Int
bit2int bits = sum [w * b | (w, b) <- zip weights bits]
                where weights = iterate (*2) 1
-}

{- This is re-written below using foldr higher order function
bit2int :: [Bit] -> Int
bit2int [] = 0
bit2int (x:xs) = x + 2 * bit2int xs
-}

-- Graham, sections about operators: page 51 pdf
-- foldr -> page 79
-- bit2int definition -> 85


bin2int :: [Bit] -> Int 
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = mod n 2 : int2bin (div n 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat.map (make8.int2bin.ord) 


chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)


decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode