module CdWars where

import Data.List

--orderedCart :: [Int] -> [(Int, Int)]
orderedCart :: [Int] -> [(Int, Int)]
orderedCart xs = [(x,y) | (x:rest) <- tails xs, y <- rest]


