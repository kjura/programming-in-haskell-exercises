module TribonacciSequence where


tribonacci :: Num a => (a, a, a) -> Int -> [a]


triboFirstTerm :: Num a => (a, a, a) -> [a]
triboFirstTerm (x, y, z) = x:(y:(z:[])) ++ [x + y + z]


tribonacci (x, y, z) 0 = []


