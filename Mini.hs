{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
module Mini where

import           Data.Bits

data B = X | O deriving Eq


instance Show B where
    show X  = "x"
    show O  = "o"


instance Show [B] where
    show [] = ""
    show (x:xs) = show x ++ show xs

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
bPad8 = bPad 8 O
bPad32 = bPad 32 O
bPad64 = bPad 64 O

bIntTo :: Int -> [B]
bIntTo 0 = []
bIntTo a = bIntTo (shiftR a 1) ++ [if odd a then X else O]
bIntTo8 = bPad8.bIntTo
bIntTo32 = bPad32.bIntTo
bIntTo64 = bPad64.bIntTo

-- bToInt :: [B] -> Int
-- bToInt [] = 0
-- bIntTo (X:xs) = bToInt xs * 2
-- bIntTo (O:xs) = bToInt xs

bToInt :: [B] -> Int
bToInt [] = 0
bToInt xs = 2 * bToInt (init xs) + bVal (last xs)


bAddNoCarry :: [B] -> [B] -> [B]
bAddNoCarry xs ys = reverse $ bAddNoCarry' (reverse xs) (reverse ys) O
bAddNoCarry' [] _ _ = []
bAddNoCarry' (x:xs) (y:ys) c1 = v : bAddNoCarry' xs ys c2
    where (c2, v) = bSAdd c1 x y

bVal :: B -> Int
bVal X = 1
bVal O = 0


bShL :: [B] -> Int -> [B]
bShL a 0 = a
bShL xs a = bShL xs (a-1) ++ [O]


bShR :: [B] -> Int -> [B]
bShR a 0 = a
bShR xs a = O : bShR (init xs) (a-1)

bXor = zipWith bSXor
bAnd = zipWith bSAnd
bOr = zipWith bSOr
bNot = map bSNot

bRotR :: [B] -> Int -> [B]
bRotR a 0 = a
bRotR xs a = bRotR (last xs : init xs) (a-1)


bRotL :: [B] -> Int -> [B]
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
bSXor X O = X
bSXor O X = X
bSXor _ _ = O

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




