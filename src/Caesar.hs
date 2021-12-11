module Caesar where

import Data.Char


-- Convert a lower-case lettter between a-z to a number from a 0-25 interval
-- ord :: Char - Int and chr :: Int -> Char convert between characters and
-- their Unicode numbers.)
let2in :: Char -> Int
let2in c = ord c - ord 'a'



-- Make the opposite of let2in
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)


-- isLower :: Char -> Bool decides if a character is a lower-case letter
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ( ( let2in c + n) `mod` 26) 
            | otherwise = c


encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
        0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
        6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]


percent :: Int -> Int -> Float
percent x y = (fromIntegral x / fromIntegral y) * 100


lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int 
count c xs = length [x | x <- xs, c == x]


freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
                where n = lowers xs

















