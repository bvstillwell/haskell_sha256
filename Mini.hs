{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
module Mini where

import           Data.Bits
import           Data.List
import           Debug.Trace

data B = O | X | V Int | Ba [B] | Bx [B] deriving (Eq, Ord)

bEdges :: B -> Int
bEdges (Ba []) = 0
bEdges (Ba (x:xs)) = 1 + bEdges x + bEdges (Ba xs)
bEdges (Bx []) = 0
bEdges (Bx (x:xs)) = 1 + bEdges x + bEdges (Bx xs)
bEdges _ = 0

bNodes :: B -> Int
bNodes (Ba []) = 0
bNodes (Ba (x:xs)) = bNodes x + bNodes (Ba xs)
bNodes (Bx []) = 0
bNodes (Bx (x:xs)) = bNodes x + bNodes (Bx xs)
bNodes _ = 1


-- bX :: B -> B -> B
-- bX a b
--     | trace (show a ++ "," ++ show b) False = undefined
--     | a > b = bX b a
--     | a == O = b
--     | a == b = O
--     | otherwise =
--         case (a, b) of (Bx c d, Bx e f) -> foldl1 bX $ sort [c, d, e, f]
--                        (c, Bx e f) -> foldl1 bX $ sort [a, e, f]
--                        (c, d) -> Bx a b

instance Show B where
    show X  = "x"
    show O  = "o"
    show (V x)  = "v" ++ show x
    show (Bx xs)  = "X(" ++ show xs ++ ")"
    show (Ba xs)  = "A(" ++ show xs ++ ")"

instance Show [B] where
    show [] = ""
    show (x:xs) =
        if null xs then show x else show x ++ ":" ++ show xs

instance Read B where
    readsPrec _ (x:xs) = [(bChrToB x, xs)]
    readList x = [(bStrToB x, "")]


bChrToB :: Char -> B
bChrToB 'x' = X
bChrToB 'o' = O


bStrToB :: String -> [B]
bStrToB = map bChrToB

bPad :: Int -> B -> [B] -> [B]
bPad i a xs = if length xs < i then a : bPad (i-1) a xs else xs
bPad4 = bPad 4 O
bPad8 = bPad 8 O
bPad32 = bPad 32 O
bPad64 = bPad 64 O

bIntTo :: Integer -> [B]
bIntTo 0 = []
bIntTo a = bIntTo (shiftR a 1) ++ [if odd a then X else O]
bIntTo4 x = bPad4 $ bIntTo x
bIntTo8 x = bPad8 $ bIntTo x
bIntTo32 x = bPad32 $ bIntTo x
bIntTo64 x = bPad64 $ bIntTo x

-- bToInt :: [B] -> Integer
-- bToInt [] = 0
-- bIntTo (X:xs) = bToInt xs * 2
-- bIntTo (O:xs) = bToInt xs

bToInt :: [B] -> Integer
bToInt [] = 0
bToInt xs = 2 * bToInt (init xs) + bVal (last xs)


bAddNoCarry :: [B] -> [B] -> [B]
bAddNoCarry xs ys = reverse $ bAddNoCarry' (reverse xs) (reverse ys) O
bAddNoCarry' [] _ _ = []
bAddNoCarry' (x:xs) (y:ys) c1 = v : bAddNoCarry' xs ys c2
    where (c2, v) = bSAdd c1 x y

bVal :: B -> Integer
bVal X = 1
bVal O = 0


bShL :: Int -> [B] -> [B]
bShL 0 a = a
bShL a xs = bShL (a-1) xs ++ [O]


bShR :: Int -> [B] -> [B]
bShR 0 a = a
bShR a xs = O : bShR (a-1) (init xs)

bXor = zipWith bSXor
bAnd = zipWith bSAnd
bOr = zipWith bSOr
bNot = map bSNot

bRotR :: Int -> [B] -> [B]
bRotR 0 a = a
bRotR a xs = bRotR (a-1) (last xs : init xs)


bRotL :: [B] -> Integer -> [B]
bRotL a 0 = a
bRotL (x:xs) a = bRotL (xs ++ [x]) (a-1)


bSAnd :: B -> B -> B
bSAnd X X = X
bSAnd _ _ = O

bSOr :: B -> B -> B
bSOr X _ = X
bSOr _ X = X
bSOr _ _ = O

bSXor :: B -> B -> B
bSXor a b
    | a > b = bSXor b a
    | a == b = O
    | a == O = b
    | otherwise = case (a, b) of
        (Bx as, Bx bs) -> Bx (as ++ bs) -- Xor 2 Xor lists
        (a', b') -> Bx [a', b']

bSNot :: B -> B
bSNot X = O
bSNot O = X

bSAdd :: B -> B -> B -> (B, B)
bSAdd O O O = (O, O)
bSAdd O O X = (O, X)
bSAdd O X O = (O, X)
bSAdd O X X = (X, O)
bSAdd X O O = (O, X)
bSAdd X O X = (X, O)
bSAdd X X O = (X, O)
bSAdd X X X = (X, X)
