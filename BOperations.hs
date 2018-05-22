module BOperations where

import           Data.Bits
import           Data.List
import           Debug.Trace
import           Text.Printf

data B = O | X | V Int | Ba B B | Bx B B | BAdd B B deriving (Eq, Ord)

instance Show B where
    show X  = "x"
    show O  = "o"
    show (V x)  = printf "%02d" x
    show (BAdd a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (Bx a b) = "(" ++ show a ++ "^" ++ show b ++ ")"
    show (Ba a b) = "(" ++ show a ++ "&" ++ show b ++ ")"

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
    | a > b = bSAnd b a
    | a == b = a
    | a == O = O
    | a == X = b
    | otherwise = case (a, b) of
        (Ba a1 b1, Ba a2 b2) -> foldl1 bSAnd [a1, b1, a2, b2]
        (a', b') -> Ba a' b'

bSXorSetRemove :: B -> [B] -> [B]
bSXorSetRemove a [] = [a]
bSXorSetRemove a (b:bc)
                | a == b    = bc
                | otherwise = b : bSXorSetRemove a bc


bX :: [B] -> B
bX [] = O
bX [a] = a

bSXor :: B -> B -> B
bSXor a b
    | a > b = bSXor b a
    | a == O = b
    | a == b = O
    | otherwise = case (a, b) of
        (Bx a1 b1, Bx a2 b2) -> foldl1 bSXor [a1, b1, a2, b2]
        (a', b') -> Bx a' b'

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
    | a > b = bSAdd b a c
    | b > c = bSAdd a c b
    | otherwise = case (a, b, c) of
        (O, O, a) -> (O, a)
        (O, a, b) -> (bSAnd a b, bSXor a b)
        (a, b, c) -> (bSAnd a b `bSXor` bSAnd b c `bSXor` bSAnd a c, foldl1 bSXor [a, b, c])

bAddNoCarry :: [B] -> [B] -> [B]
bAddNoCarry xs ys = reverse (bAddNoCarry' (reverse xs) (reverse ys) O)
bAddNoCarry' [] _ _ = []
bAddNoCarry' (x:xs) (y:ys) c1 = v : bAddNoCarry' xs ys c2
    where (c2, v) = bSAdd c1 x y
