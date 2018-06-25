module BOperations where

import           Data.Bits
import           Data.List
import           Debug.Trace
import           Text.Printf

data N = I | N deriving (Eq, Ord, Show) -- Normal, Inverted
data B = O | X | V Int N | Ba B B | Bx B B deriving (Eq, Ord, Show)

bValid :: B -> B  -- A function to throw errors on bad B
bValid a =
    let bError a = error $ "INVALID:\n" ++ showPretty a in
    case a of
    (Ba X _) -> bError a
    (Ba O _) -> bError a
    (Ba _ X) -> bError a
    (Ba _ O) -> bError a
    (Ba Ba{} _) -> bError a
    (Ba (Bx _ _) _) -> bError a
    (Ba _ (Bx _ _)) -> bError a
    (Bx X _) -> bError a
    (Bx O _) -> bError a
    (Bx _ X) -> bError a
    (Bx _ O) -> bError a
    (Bx (Bx _ _) _) -> bError a
    -- (Bx Ba{} _) -> bError a
    -- (Bx _ Ba{}) -> bError a
    a -> a

nInv N = I
nInv I = N

bShL :: Int -> [B] -> [B]
bShL 0 a = a
bShL a xs = bShL (a-1) xs ++ [O]

bShR :: Int -> [B] -> [B]
bShR 0 a = a
bShR a xs = O : bShR (a-1) (init xs)

bXor = zipWith bSXor
bAnd = zipWith bSAnd
bNot = map bSNot

bRotR :: Int -> [B] -> [B]
bRotR 0 a = a
bRotR a xs = bRotR (a-1) (last xs : init xs)


bRotL :: [B] -> Integer -> [B]
bRotL a 0 = a
bRotL (x:xs) a = bRotL (xs ++ [x]) (a-1)



bSNot :: B -> B
bSNot a
    -- | trace ("(bSNot (" ++ show a ++ ")") False = undefined
    | otherwise = bSNot' a
bSNot' X = O
bSNot' O = X
bSNot' (V a n) = V a (nInv n)
bSNot' (Bx a b) = Bx (bSNot a) (bSNot b)
bSNot' (Ba a b) = Ba (bSNot a) (bSNot b)


bSor :: B -> B -> B
bSor a b
    | a > b = bSor b a
    | a == O = b
    | a == X = b

bSAdd :: B -> B -> B -> (B, B)
-- Result is (carry, value)
--           (a&b xor b&c xor a&c, xor (a,b,c))
bSAdd a b c
    -- | trace ("(bSAdd " ++ show a ++ " " ++ show b ++ " " ++ show c ++ ")") False = undefined
    | a > b = bSAdd b a c
    | b > c = bSAdd a c b
    | otherwise = case (a, b, c) of
        (O, O, a) -> (O, a)
        (O, a, b) -> (bSAnd a b, bSXor a b)
        (a, b, c) -> (bSAnd a b `bSXor` (bSAnd b c `bSXor` bSAnd a c), foldl1 bSXor [a, b, c])


bAddNoCarry :: [B] -> [B] -> [B]
-- Add from left to right
bAddNoCarry xs ys = snd $ bAddLeftToRight xs ys

bAddLeftToRight :: [B] -> [B] -> (B, [B])
-- Add from left to right
-- Result (carry, value)
bAddLeftToRight [] _ = (O, [])
bAddLeftToRight (x:xs) (y:ys) =
    let
        -- Get the previous add carry
        (c', vs') = bAddLeftToRight xs ys
        -- Add our current values
        (c, v) = bSAdd x y c'
    in
        -- Return the result
        (c, v:vs')

-- Create an and tree
bSAnd :: B -> B -> B
bSAnd a b
    -- | trace ("(bSAnd (" ++ show a ++ ") (" ++ show b ++ "))") False = undefined
    | a > b = bSAnd b a  -- Get the correct order
    | a == b = a  -- Ignore duplicates
    | a == O = O  -- O finishes it
    | a == X = b  -- X ignored
    | a == bSNot b = O  -- X ignored
    | otherwise = bValid $ bSAnd' a b
-- O *
-- X *
-- V V
bSAnd' (V a1 s1) (V a2 s2) = Ba (V a1 s1) (V a2 s2)
-- V Ba
bSAnd' (V a1 s1) (Ba b c) =
    let
        a = V a1 s1
        r = bSTestNode3 a b c
    in case r of
        EQUALAB -> Ba b c
        EQUALAC -> Ba b c
        OK -> Ba a (Ba b c)
        INVERSEAB -> O
        INVERSEAC -> O
        REORDER -> bSRecurseAndEval a b c bSAnd Ba
-- Ba Ba (Illegal structure rebuild)
bSAnd' (Ba a1 b1) (Ba a2 b2) =  foldr1 bSAnd (sort [a1, b1, a2, b2])
-- V Bx
bSAnd' a (Bx b c) = bSXor (bSAnd a b) (bSAnd a c)

bSAnd' a b = bSAndError' a b ""
bSAndError' a b c = error $ "Undefined\n  bSAnd \n    (" ++ show a ++ ")\n    (" ++ show b ++ ")" ++ show c







-- Create an and tree
bSXor :: B -> B -> B
bSXor a b
    -- | trace ("(bSXor (" ++ show a ++ ") (" ++ show b ++ "))") False = undefined
    | a > b = bSXor b a  -- Get the correct order
    | a == b = O  -- Dupliates Cancel
    | a == O = b  -- O Ignore
    | a == X = bSNot b  -- X is invert
    | a == bSNot b = X  -- Xor cancelation (a ^ (X ^ a) -> a ^ a ^ x -> X)
    | otherwise = bValid $ bSXor' a b
-- Dealt with
-- O *
-- X *
-- V V
bSXor' (V a1 s1) (V a2 s2) = Bx (V a1 s1) (V a2 s2)

-- V Ba
bSXor' (V a1 s1) (Ba b c) =
    let
        a = V a1 s1
        r = bSTestNode3 a b c
    in case r of
        EQUALAB -> Bx a (Ba b c)
        -- EQUALAC -> Bx a (Ba b c)
        INVERSEAB -> bSXor a (bSXor c (bSAnd a c))
        -- INVERSEAC -> bSXor a (bSXor b (bSAnd a b))
        OK -> Bx a (Ba b c)
        REORDER -> bSRecurseAndEval a b c bSXor Bx
        _ -> bSXorError' (V a1 s1) (Ba b c) (show r)

-- V Bx
bSXor' (V a1 s1) (Bx b c) =
    let
        a = V a1 s1
        r = bSTestNode3 a b c
    in case r of
        EQUALAB -> c -- Xor cancel
        -- EQUALAC -> b -- Xor cancel
        INVERSEAB -> bSNot c
        -- INVERSEAC -> bSNot b
        OK -> Bx a (Bx b c)
        REORDER -> bSRecurseAndEval a b c bSXor Bx
        _ -> bSXorError' (V a1 s1) (Bx b c) (show r)

-- Bx Bx (Illegal structure)
bSXor' (Bx a1 b1) (Bx a2 b2) =  foldr1 bSXor (sort [a1, b1, a2, b2])

-- Ba Ba
bSXor' (Ba a1 b1) (Ba a2 b2) =
    let
        a = Ba a1 b1
        b = Ba a2 b2
        rab = bSTestNodes a b
    in case rab of
        -- EQUALAB -> O
        -- INVERSEAB -> X
        OK -> Bx a b
        -- _  -> bSXorError' a b (show rab)

-- Bx Ba
bSXor' (Ba a1 b1) (Bx b c) =
    let
        a = Ba a1 b1
        rab = bSTestNodes a b
        rac = bSTestNodes a c
    in case (rab, rac) of
        (EQUALAB, _) -> b
        (_, EQUALAB) -> a
        (INVERSEAB, _) -> bSNot c
        (_, INVERSEAB) -> bSNot b
        (OK, OK) -> Bx a (Bx b c)
        (REORDER, _) -> foldr1 bSXor $ sort [a, b, c]
        (_, REORDER) -> foldr1 bSXor $ sort [a, b, c]
        (_, _) -> bSXorError' (Ba a1 b1) (Bx b c) (show (rab, rac))

-- -- Bx Ba
-- bSXor' (Bx a b) (Ba a1 b1) =
--     let
--         c = Ba a1 b1
--         rac = bSTestNodes a c
--         rbc = bSTestNodes b c
--     in case (rac, rbc) of
--         -- (EQUALAB, _) -> b
--         -- (_, EQUALAB) -> a
--         -- (INVERSEAB, _) -> bSNot b
--         (_, INVERSEAB) -> bSNot a
--         (OK, OK) -> Bx a (Bx b c)
--         (_, _) -> bSXorError' (Bx a b) c (show (rac, rbc))

bSXor' a b = bSXorError' a b ""
bSXorError' a b c = error $ "Undefined\n  bSXor \n    (" ++ show a ++ ")\n    (" ++ show b ++ ")" ++ c








data NodeTestResult =
    EQUALAB
    | EQUALAC
    | EQUALAD
    | EQUALBC
    | EQUALBD
    | INVERSEAB
    | INVERSEAC
    | INVERSEAD
    | INVERSEBC
    | INVERSEBD
    | REORDER  -- a reorder is required
    | OK deriving (Eq, Show)

bSTestNodes :: B -> B -> NodeTestResult
bSTestNodes a b = bSTestNodes' (compare a b) (compare (bSNot a) b)
bSTestNodes' _  EQ = INVERSEAB
bSTestNodes' LT _  = OK
bSTestNodes' EQ _  = EQUALAB
bSTestNodes' _  _  = REORDER

bSTestNode3 :: B -> B -> B -> NodeTestResult
bSTestNode3 a b c =
    let
        n2 = bSTestNodes a b
        ac = compare a c
        bc = compare b c
        ai = bSNot a
        bi = bSNot b
        aic = compare ai c
        bic = compare bi c
    in if n2 /= OK then n2 else bSTestNode3' ac bc aic bic
bSTestNode3' _  _  EQ _  = INVERSEAC
bSTestNode3' _  _  _  EQ = INVERSEBC
bSTestNode3' LT _  _  _  = OK
bSTestNode3' EQ _  _  _  = EQUALAC
bSTestNode3' _  EQ _  _  = EQUALBC
bSTestNode3' _  _  _  _  = REORDER

bSTestNode4 :: B -> B -> B -> B -> NodeTestResult
bSTestNode4 a b c d =
    let
        n3 = bSTestNode3 a b c
        ad = compare a d
        bd = compare b d
        ai = bSNot a
        bi = bSNot b
        aid = compare ai d
        bid = compare bi d
    in if n3 /= OK then n3 else bSTestNode4' ad bd aid bid
bSTestNode4' _  _  EQ _  = INVERSEAD
bSTestNode4' _  _  _  EQ = INVERSEBD
bSTestNode4' LT LT _  _  = OK
bSTestNode4' EQ _  _  _  = EQUALAD
bSTestNode4' _  EQ _  _  = EQUALBD
bSTestNode4' _  _  _  _  = REORDER


bSRecurseAndEval a b c evalFunc createFunc =
    let
        result = evalFunc a c -- Calculate the result from up the tree
        expectedResult = createFunc a c
    in
        if result == expectedResult -- Does it require a recalc?
        then createFunc b expectedResult -- No it was as expected
        else evalFunc b result -- Yes, this might influence backwards

showPretty :: B -> String
showPretty a = showPretty' a ""
showPretty' (Ba a b) spaces = showPretty'' "bSAnd" a b spaces
showPretty' (Bx a b) spaces = showPretty'' "bSXor" a b spaces
showPretty' a spaces = spaces ++ show a
showPretty'' t (V a1 s1) (V a2 s2) spaces =
        spaces ++ t ++ " (" ++ show (V a1 s1) ++ ") (" ++ show (V a2 s2) ++ ")"
showPretty'' t a b spaces =
        spaces ++ "(" ++ t ++ " \n" ++
    showPretty' a (spaces ++ "  ") ++ "\n" ++
    showPretty' b (spaces ++ "  ") ++ "\n" ++
    spaces ++ ")"

