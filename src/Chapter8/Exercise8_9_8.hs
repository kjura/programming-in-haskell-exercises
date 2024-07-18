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


-- Values for tests
p1 :: Proposition
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Proposition
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Proposition
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Proposition
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')


eval :: Substitution -> Proposition -> Bool
eval _ (Const b) = b
eval sub (Var c) = find c sub
eval sub (Not prop) = not (eval sub prop)
eval sub (And a b) = eval sub a && eval sub b
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
variablesInTheProposition (Imply propA propB) = variablesInTheProposition propA ++ variablesInTheProposition propB


bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range
    where
        range = [0..(2^n)-1]
        make n bs = take n (bs ++ repeat 0)
        conv 0 = False
        conv 1 = True
        conv _ = False