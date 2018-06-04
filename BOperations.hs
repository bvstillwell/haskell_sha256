module BOperations where

import           Data.Bits
import           Data.List
import           Debug.Trace
import           Text.Printf

data B = O | X | V Int | Bx B B | Ba B B | BAdd B B deriving (Eq, Ord)

instance Show B where
    -- show X  = "x"
    -- show O  = "o"
    -- show (V x)  = printf "%02d" x
    -- show (BAdd a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    -- show (Bx a b) = "(" ++ show a ++ "^" ++ show b ++ ")"
    -- show (Ba a b) = "(" ++ show a ++ "&" ++ show b ++ ")"
    show X  = "X"
    show O  = "O"
    show (V x)  = printf "(V %d)" x
    -- show (BAdd a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (Bx a b) = "(Bx " ++ show a ++ " " ++ show b ++ ")"
    show (Ba a b) = "(Ba " ++ show a ++ " " ++ show b ++ ")"

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


bSAnd :: B -> B -> B
bSAnd a b
    -- | trace ("(bSAnd " ++ show a ++ " " ++ show b ++ ")") False = undefined
    | a > b = bSAnd b a  -- Get the correct order
    | a == b = a  -- Ignore duplicates
    | a == O = O  -- O finishes it
    | a == X = b  -- X ignored
    | otherwise = bSAnd' a b
bSAnd' (Ba a1 b1) (Ba a2 b2) = foldr1 bSAnd (sort [a1, b1, a2, b2])
bSAnd' (Ba _ _) _ = error "Not supposed to have this"
bSAnd' a (Ba b c)
    | a == b = Ba b c
    | a == c = Ba b c
    | b >= c = error $ "Incorrect order in Ba" ++ show a ++ "(Ba " ++ show b ++ " " ++ show c ++ ")"
    | b == X = error "X in Ba"
    | b == O = error "O in Ba"
    | a > b = bSAnd b (bSAnd a c)
    | a > c = bSAnd b (bSAnd c a)
    | otherwise = Ba a (Ba b c) -- All in order
bSAnd' a (Bx X b) -- Special cancel a & !a = O
    | a == b = O
    | otherwise = Bx a (Bx X b)
bSAnd' X a = error "Unexpected X"
bSAnd' a b = Ba a b

bSXor :: B -> B -> B
bSXor a b
    | trace ("(bSXor " ++    show a ++ " " ++ show b ++ ")") False = undefined
    | a > b = bSXor b a -- Get the correct order
    | a == O = b  -- Xor ignore
    | a == b = O  -- Xor kill
    | otherwise = bSXor' a b
bSXor' (Bx a1 b1) (Bx a2 b2) = foldr1 bSXor (sort [a1, b1, a2, b2]) -- Not allowed structure!
bSXor' a (Bx b c) -- Kill some values, then redorder correctly (Assumed b < c)
    | a == b = c  -- Xor kill
    | a == c = b  -- Xor kill
    | b >= c = error $ "Incorrect order in bSXor " ++ show a ++ " (Bx " ++ show b ++ " " ++ show c ++ ")"
    -- Get the ordering. Assumed b < c and cancelations have been done
    | a > b = bSXor b (bSXor a c) -- Change the order. Right could return O
    | a > c = bSXor b (bSXor c a) -- Change the order. Right might return O
    | otherwise = bSXor1' a (Bx b c)  -- Order is OK!
bSXor' (Bx a b) c = bSXor' c (Bx a b) -- Let's use our other func to order
bSXor' a b = bSXor1' a b

bSXor1' X (V a) = Bx X (V a)
bSXor1' X (Bx a b) = Bx X (Bx a b)
bSXor1' (V a) (V b) = Bx (V a) (V b)
bSXor1' (Ba a1 b1) (Ba a2 b2) = Bx (Ba a1 b1) (Ba a2 b2)
bSXor1' a (Ba a2 b2) = Bx a (Ba a2 b2)
-- bSXor' a b = Bx a b  -- We're happy with what they sent in


bX :: [B] -> B
bX [] = O
bX [a] = a
-- bSXor (Bx X (V 1)) (Ba (V 1) (V 2))


bSNot :: B -> B
bSNot X = O
bSNot O = X
bSNot a = bSXor X a

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
