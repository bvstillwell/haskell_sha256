{-# LANGUAGE FlexibleInstances #-}
module Convert where

import           BOperations
import           Data.Bits
import           Data.Char       (intToDigit, ord)
import           Data.List.Split

instance Show B where
    show X  = "x"
    show O  = "o"
    show (V x)  = "v" ++ show x
    show (Bx xs)  = "(" ++ show (xs, "^") ++ ")"
    show (Ba xs)  = "(" ++ show (xs, "&") ++ ")"

instance {-# OVERLAPS #-} Show ([B], String) where
    show (x : xs, a) =
        if null xs then show x else show x ++ a ++ show (xs, a)

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
-- bPad4 = bPad 4 O
bPad8 = bPad 8 O
-- bPad32 = bPad 32 O
bPad64 = bPad 64 O

bIntTo :: Integer -> [B]
bIntTo 0 = []
bIntTo a = bIntTo (shiftR a 1) ++ [if odd a then X else O]
-- bIntTo4 x = bPad4 $ bIntTo x
bIntTo8 x = bPad8 $ bIntTo x
-- bIntTo32 x = bPad32 $ bIntTo x
bIntTo64 x = bPad64 $ bIntTo x

bVal :: B -> Integer
bVal X = 1
bVal O = 0

bToInt :: [B] -> Integer
bToInt [] = 0
bToInt xs = 2 * bToInt (init xs) + bVal (last xs)

textToChunks :: String -> Int -> [[[B]]]
textToChunks theText size =
    let
        fourth = quot size 4
        eighth = quot size 8

        bPadInt x = bPad fourth O (bIntTo x)
        -- Pre-processing:
        -- begin with the original message of length L bits
        -- append a single '1' bit
        -- append K '0' bits, where K is the minimum number >= 0 such that L + 1 + K + 64 is a multiple of 512
        -- append L as a 64-bit big-endian integer, making the total post-processed length a multiple of 512 bits
        msg = concatMap (bIntTo8.toInteger.ord) theText
        addZeros = (size * 2) - mod (length msg + 1 + fourth) (size * 2)
        -- Create the initial msg
        msg2 = msg ++ [X] ++ replicate addZeros O ++ (bPadInt.toInteger.length) msg
        -- Create 512byte chunks.
        chunksLarge = chunksOf (size * 2) msg2
    in
        map (chunksOf eighth) chunksLarge
