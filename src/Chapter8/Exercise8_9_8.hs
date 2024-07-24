module Exercise_8_9_8 where

type Bit = Int

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = mod n 2 : int2bin (div n 2)

-- Declare a lookup table
type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k'] -- not safe if nothing in the lookup table

type Substitution = Assoc Char Bool

-- Declare a proposistion data type

data Proposition = Const Bool 
                | Var Char
                | Not Proposition
                | And Proposition Proposition
                | Imply Proposition Proposition
                | Or Proposition Proposition


-- Values for tests
p1 :: Proposition
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Proposition
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Proposition
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Proposition
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

lawOfTheExcludedMiddle :: Proposition
lawOfTheExcludedMiddle = Or (Var 'A') (Var 'B')

lawOfNoncontradiction :: Proposition
lawOfNoncontradiction = Not (And (Var 'p') (Not (Var 'p')))



eval :: Substitution -> Proposition -> Bool
eval _ (Const b) = b
eval sub (Var c) = find c sub
eval sub (Not prop) = not (eval sub prop)
eval sub (And a b) = eval sub a && eval sub b
eval sub (Or a b) = eval sub a || eval sub b
eval sub (Imply a b) = eval sub a <= eval sub b -- Still kinda mysterious el-oh-el
{-  
eval [('A', True), ('B', False), ('E', True)] Imply (Var 'A') (And (Var 'A') (Var 'B'))

= eval [('A', True), ('B', False), ('E', True)] (Var 'A') <= eval [('A', True), ('B', False), ('E', True)] (And (Var 'A') (Var 'B'))
= True <= (eval [('A', True), ('B', False), ('E', True)] Var 'A' && eval [('A', True), ('B', False), ('E', True)] Var 'B'))
= True <= (True && False) = True <= (False) = False
-}

-- Get list of all boolean variables inside a proposition
variablesInTheProposition :: Proposition -> [Char]
variablesInTheProposition (Const _) = []
variablesInTheProposition (Var b) = [b]
variablesInTheProposition (Not prop) = variablesInTheProposition prop
variablesInTheProposition (And propA propB) = variablesInTheProposition propA ++ variablesInTheProposition propB
variablesInTheProposition (Or propA propB) = variablesInTheProposition propA ++ variablesInTheProposition propB
variablesInTheProposition (Imply propA propB) = variablesInTheProposition propA ++ variablesInTheProposition propB


bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range
    where
        range = [0..(2^n)-1]
        make n bs = take n (bs ++ repeat 0)
        conv 0 = False
        conv 1 = True
        conv _ = False

{- 

bools 1 = [(reverse . map conv . make n . int2bin) 0, (reverse . map conv . make n . int2bin) 1]
= []

remeber that bools 1 when evaluating for 0 we have => take 1 ([] ++ repeat 0) = [0]
n = 1 for make n bs

bools 3 = map (reverse . map conv . make 3 . int2bin) [0..(2^n)-1] =
= map (reverse . map conv . make 3 . int2bin) [0..7] = 
= map ( reverse ( map conv ( make 3 ( int2bin ) ) ) ) [0..7]
= [( reverse ( map conv ( make 3 ( int2bin ) ) ) ) 0, ( reverse ( map conv ( make 3 ( int2bin ) ) ) ) 1, ( reverse ( map conv ( make 3 ( int2bin ) ) ) ) 2, ... ]

(.)(0) --> ( reverse ( map conv ( make 3 ( int2bin 0) ) ) ) = ( reverse ( map conv ( make 3 ( []) ) ) ) 
= ( reverse ( map conv ( take 3 ([] ++ repeat 0) ) ) ) = ( reverse ( map conv ( take 3 ([] ++ repeat 0) ) ) )

= ( reverse ( map conv [0, 0, 0] ) ) = reverse ( [conv 0, conv 0, conv 0] ) = reverse ( [False, False, False] )
= [False, False, False]

(.)(0) --> bools 1 = [(reverse . map conv . make n . int2bin) 0, (reverse . map conv . make n . int2bin) 1]
= 

bools 2 = map (reverse . map conv . make 3 . int2bin) [0, 1, 2, 3]
= [(reverse . map conv . make 2 . int2bin) 0, (reverse . map conv . make 2 . int2bin) 1, (reverse . map conv . make 2 . int2bin) 2, (reverse . map conv . make 2 . int2bin) 3]

-}

simpleBools :: Int -> [[Bool]]
simpleBools 0 = [[]]
simpleBools n = map (False :) bss ++ map (True :) bss
    where bss = simpleBools (n - 1)

-- example
-- simpleBools 2 = 
--             map (False :) simpleBools (1) ++ map (True :) simpleBools (1) =
--             = map (False :) ( map (False :) simpleBools (0) ++ map (True :) simpleBools (0)
--  ) ++ map (True :) ( map (False :) simpleBools (0) ++ map (True :) simpleBools (0)
--  )          = map (False :) ( map (False :) [[]] ++ map (True :) [[]]
--  ) ++ map (True :) ( map (False :) [[]] ++ map (True :) [[]]
--  ) = map (False :) ( [[False]] ++ [[True]] ) ++ map (True :) ( ( [[False]] ++ [[True]] ) )
--  = [[False, False], [False, True]] ++ [[True, False], [True, True]]
--  = [[False, False], [False, True], [True, False], [True, True]]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)


substs :: Proposition -> [Substitution]
substs p = map (zip vs) (bools (length vs))
    where vs = rmdups (variablesInTheProposition p)

-- variablesInTheProposition (Imply (Not (Var 'A')) (And (Var 'B') (Var 'B')))
--  map (zip "AB") [[False,False],[False,True],[True,False],[True,True]]
-- = [(zip "AB") [False,False], (zip "AB") [False,True], (zip "AB") [True,False], (zip "AB") [True,True]]
-- = [[('A',False),('B',False)],[('A',False),('B',True)],[('A',True),('B',False)],[('A',True),('B',True)]]

isTaut :: Proposition -> Bool
isTaut prop = and [eval sub prop | sub <- substs prop]
-- Or (Var 'p') (Not (Var 'p'))