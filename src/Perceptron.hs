module Perceptron where

import qualified Data.Vector as V
import System.Random

main = do
  g <- getStdGen
  print . take 10 $ (randomRs ('a', 'z') g)
  print . take 10 $ (randomRs ('a', 'z') g)
 
  



  
