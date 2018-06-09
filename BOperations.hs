module BOperations where

import           Data.Bits
import           Data.List
import           Debug.Trace
import           Text.Printf

data N = I | N deriving (Eq, Ord, Show) -- Normal, Inverted
data B = O | X | V Int N | Bx B B | Ba B B deriving (Eq, Ord, Show)

-- instance Show B where
--     show X  = "X"
--     show O  = "O"
--     show (V x a)  = printf "(V %d %s)" x a
--     show (Bx a b) = "(Bx " ++ show a ++ " " ++ show b ++ ")"
--     show (NBx a b) = "(NBx " ++ show a ++ " " ++ show b ++ ")"
--     show (Ba a b) = "(Ba " ++ show a ++ " " ++ show b ++ ")"
--     show (NBa a b) = "(NBa " ++ show a ++ " " ++ show b ++ ")"

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

-- Create an and tree
bSAnd :: B -> B -> B
bSAnd a b
    -- | trace ("(bSAnd (" ++ show a ++ ") (" ++ show b ++ "))") False = undefined
    | a > b = bSAnd b a  -- Get the correct order
    | a == b = a  -- Ignore duplicates
    | a == O = O  -- O finishes it
    | a == X = b  -- X ignored
    | otherwise = bSAnd' a b
-- Dealt with
-- O *
-- X *
bSAnd' (V a1 s1) (V a2 s2)
    | a1 == a2 = O -- Invert && Not invert = O. Inferred as a == b is canceled in the first part
    | otherwise = Ba (V a1 s1) (V a2 s2)
-- V V
bSAnd' (V a1 s1) (Ba (V a2 s2) c)
    | V a1 s1 == V a2 s2 = Ba (V a2 s2) c -- Ignore it
    | V a1 s1 == c = Ba (V a2 s2) c -- Ignore it
    | a1 == a2 && s1 == nInv s2 = O -- And a1 a2 kill!
    -- Standard graph manipulation
    | a1 < a2 = Ba (V a1 s1) (Ba (V a2 s2) c) -- We've found our place. Add to list
    | otherwise = -- Recurse to the next location in list. a1 > a2
        let
            result = bSAnd (V a1 s1) c -- Calculate the result from up the tree
            unchanged = Ba (V a1 s1) c
        in
            if result == unchanged -- Does it require a recalc?
            then Ba (V a2 s2) unchanged -- No it was as expected
            else bSAnd (V a2 s2) result -- Yes, this might influence backwards
bSAnd' (Bx (V a1 s1) (V a2 s2)) (Ba (V a3 s3) (V a4 s4))
    -- (a and b) xor (c and d)
    -- bSAnd (Bx (V 1 N) (V 2 N)) (Ba (V 1 N) (V 2 I))
    | a == c && b == bSNot d = bSAnd a (bSNot b)

    -- bSAnd (Bx (V 1 N) (V 2 N)) (Ba (V 1 I) (V 2 I))
    | a == bSNot c && b == bSNot d = O

    | otherwise = bSAndError' (Bx (V a1 s1) (V a2 s2)) (Ba (V a3 s3) (V a4 s4))
    where
        a = V a1 s1
        b = V a2 s2
        c = V a3 s3
        d = V a4 s4
-- V BaV
-- Create the from the 4 values
bSAnd' (Ba a1 b1) (Ba a2 b2) =  foldr1 bSAnd (sort [a1, b1, a2, b2])
bSAnd' a b = bSAndError' a b
bSAndError' a b = error $ "Undefined bSAnd (" ++ show a ++ ") (" ++ show b ++ ")"




-- Create an and tree
bSXor :: B -> B -> B
bSXor a b
    -- | trace ("(bSXor (" ++ show a ++ ") (" ++ show b ++ "))") False = undefined
    | a > b = bSXor b a  -- Get the correct order
    | a == b = O  -- Dupliates Cancel
    | a == O = b  -- O Ignore
    | a == X = bSNot b  -- X is invert
    | otherwise = bSXor' a b
-- Dealt with
-- O *
-- X *
bSXor' (V a1 s1) (V a2 s2)
    | a1 == a2 = X -- Invert && Not invert = x. Inferred as a == b is canceled in the first part
    | otherwise = Bx (V a1 s1) (V a2 s2)
-- V V
bSXor' (V a1 s1) (Bx (V a2 s2) c)
    | V a1 s1 == V a2 s2 = c -- Kill first
    | V a1 s1 == c = V a2 s2 -- Kill second
    | a1 == a2 && s1 == nInv s2 = X -- Return an inverse
    -- Standard graph manipulation
    | a1 < a2 = Bx (V a1 s1) (Bx (V a2 s2) c) -- We've found our place. Add to list
    | otherwise = -- Recurse to the next location in list. a1 > a2
        let
            result = bSXor (V a1 s1) c -- Calculate the result from up the tree
            unchanged = Bx (V a1 s1) c
        in
            if result == unchanged -- Does it require a recalc?
            then Bx (V a2 s2) unchanged -- No it was as expected
            else bSXor (V a2 s2) result -- Yes, this might influence backwards

bSXor' (V a1 s1) (Ba (V a2 s2) (V a3 s3))
    | a == b = bSAnd b (bSNot c) -- a xor (a and b) = a and !b
    | a == c = bSAnd c (bSNot b)
    | a == bSNot b = bSNot (bSAnd a c) -- a xor (a and !b) = ! (a and b)
    | a == bSNot c = bSNot (bSAnd a b)
    | otherwise = bSXorError' a (Ba b c)
    where
        a = V a1 s1
        b = V a2 s2
        c = V a3 s3
bSXor' (Ba (V a1 s1) (V a2 s2)) (Ba (V a3 s3) (V a4 s4))
    -- (a and b) xor (c and d)
    -- => a == c && b == !d = a
    | a == c && b == bSNot d = a
    | a == d && b == bSNot c = a
    | b == c && a == bSNot d = b
    | b == d && a == bSNot c = b
    | otherwise = bSXorError' (Ba a b) (Ba c d)
    where
        a = V a1 s1
        b = V a2 s2
        c = V a3 s3
        d = V a4 s4
bSXor' (Bx (V a1 s1) (V a2 s2)) (Ba (V a3 s3) (V a4 s4))
    -- (a xor b) xor (c and d)
    -- bSXor (Bx (V 1 I) (V 2 I)) (Ba (V 1 N) (V 2 I))
    | a == bSNot c && b == d = bSAnd a (bSNot b)
    | otherwise = bSXorError' (Ba a b) (Ba c d)
    where
        a = V a1 s1
        b = V a2 s2
        c = V a3 s3
        d = V a4 s4

-- V BaV
-- Create the from the 4 values
bSXor' (Bx a1 b1) (Bx a2 b2) =  foldr1 bSXor (sort [a1, b1, a2, b2])
bSXor' a b = bSXorError' a b
bSXorError' a b = error $ "Undefined bSXor (" ++ show a ++ ") (" ++ show b ++ ")"



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

bSCarry2' :: [B] -> [B] -> (B, Maybe [B])
-- Return the carry, and the underlying values if calculated
-- Will recursively stop when trying to add O O
bSCarry2' [x] [y] = let (c, v) = bSAdd x y O in (c, Just [v])
bSCarry2' (O:xs) (O:ys) = (O, Nothing) -- We don't need to recurse
bSCarry2' (x:xs) (y:ys) =
    let
        (c', vs') = bSCarry2' xs ys
        (c, v) = bSAdd x y c'
    in
        if vs' == Nothing
        then (c, Nothing)
        else let Just vs = vs' in (c, Just (v:vs))

-- bAddNoCarry xs ys = reverse (bAddNoCarry' (reverse xs) (reverse ys) O)
-- bAddNoCarry' [] _ _ = []
-- bAddNoCarry' (x:xs) (y:ys) c1 = v : bAddNoCarry' xs ys c2
--     where (c2, v) = bSAdd c1 x y

-- bSAddWithout :: B -> B -> B -> Bool
-- bSAddWithout a b c
--     | a > b = bSAddWithout b a c
--     | b > c = bSAddWithout a c b
--     | otherwise = case (a, b, c) of
--         (O, _, _) -> False
--         (X, _, _) -> False
--         (V a, _, _) -> False
--         _ -> True
