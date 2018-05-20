module BOperations where

import           Data.Bits
import           Data.List
import           Debug.Trace

data B = O | X | V Int | Ba B B | Bx B B deriving (Eq, Ord)

bAddNoCarry :: [B] -> [B] -> [B]
bAddNoCarry xs ys = reverse (bAddNoCarry' (reverse xs) (reverse ys) O)
bAddNoCarry' [] _ _ = []
bAddNoCarry' (x:xs) (y:ys) c1 = v : bAddNoCarry' xs ys c2
    where (c2, v) = bSAdd c1 x y

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
-- bSAdd O O O = (O, O)
-- bSAdd O O X = (O, X)
-- bSAdd O X O = (O, X)
-- bSAdd O X X = (X, O)
-- bSAdd X O O = (O, X)
-- bSAdd X O X = (X, O)
-- bSAdd X X O = (X, O)
-- bSAdd X X X = (X, X)
bSAdd a b c = (bSAnd a b `bSXor` bSAnd b c `bSXor` bSAnd a c, foldl1 bSXor [a, b, c])
