module SHAHelper where

import           Data.Bits
import           Data.Char       (intToDigit, ord)
import           Data.List.Split
import           Debug.Trace
import           Mini
import           Text.Printf

textToChunks :: Int -> String -> [[[B]]]
textToChunks size theText = let
    -- Pre-processing:
    -- begin with the original message of length L bits
    -- append a single '1' bit
    -- append K '0' bits, where K is the minimum number >= 0 such that L + 1 + K + 64 is a multiple of 512
    -- append L as a 64-bit big-endian integer, making the total post-processed length a multiple of 512 bits
    msg = concatMap (bIntTo8.toInteger.ord) theText
    addZeros = (size * 2) - mod (length msg + 1 + sizeDiv4) (size * 2)
    -- Create the initial msg
    msg2 = msg ++ [X] ++ replicate addZeros O ++ (bPadInt.toInteger.length) msg
    -- Create 512byte chunks.
    chunksLarge = chunksOf (size * 2) msg2
        in map (chunksOf sizeDiv8) chunksLarge
    where
        sizeDiv4 = size `shiftR` 2
        sizeDiv8 = size `shiftR` 3
        bPadInt x = bPad sizeDiv4 O (bIntTo x)

-- extend W step
extendWBase :: (Int, Int, Int) -> [B] -> [B]
extendWBase (a, b, c) x = bRotR a x `bXor` bRotR b x `bXor` bShR c x

extendWStep :: ([B] -> [B]) -> ([B] -> [B]) -> [[B]] -> [B]
extendWStep s0 s1 ws =
    let l = length ws
    in     (ws !! (l-16)) `bAddNoCarry`
        s0 (ws !! (l-15)) `bAddNoCarry`
           (ws !! (l -7)) `bAddNoCarry`
        s1 (ws !! (l -2))

extendW :: ([[B]] -> [B]) -> Int -> [[B]] -> [[B]]
extendW extendWStep steps chunks = foldl (\wAcc _ -> wAcc ++ [extendWStep wAcc]) chunks [1..steps]


-- Compress H func
compressHBase :: (Int, Int, Int) -> [B] -> [B]
compressHBase (a, b, c) x = bRotR a x `bXor` bRotR b x `bXor` bRotR c x

compressHStep :: ([B] -> [B]) -> ([B] -> [B]) ->  [[B]] -> [B] -> [B] -> [[B]]
compressHStep s0 s1 [a, b, c, d, e, f, g, h] k w =
    let
        ch = (e `bAnd` f) `bXor` (bNot e `bAnd` g)
        temp1 = h `bAddNoCarry` s1 e `bAddNoCarry` ch `bAddNoCarry` k `bAddNoCarry` w
        maj = (a `bAnd` b) `bXor` (a `bAnd` c) `bXor` (b `bAnd` c)
        temp2 = s0 a `bAddNoCarry` maj
    in
        [temp1 `bAddNoCarry` temp2, a, b, c, d `bAddNoCarry` temp1, e, f, g]

compressH :: ([[B]] -> [B] -> [B] -> [[B]]) -> ([[B]] -> [B]) -> [[B]] -> [[B]] -> [[B]] -> [[B]]
compressH compressHStep extendWStep kSeed h w =
    let extendW' = extendW extendWStep (length kSeed) w
        zipKSeedW = zip kSeed extendW'
        hResult = foldl (\hAcc (k, w) -> compressHStep hAcc k w) h zipKSeedW
    in
        zipWith bAddNoCarry h hResult

textToSha :: ([[B]] -> [B] -> [B] -> [[B]]) -> ([[B]] -> [B]) -> Int -> [[B]] -> [[B]] -> String -> String
-- textToSha compressHStep extendWStep size kSeed hSeed theText | trace ("hSeed" ++ show (hSeed)) False = undefined
-- textToSha compressHStep extendWStep size kSeed hSeed theText | trace ("kSeed" ++ show (kSeed)) False = undefined
textToSha compressHStep extendWStep size kSeed hSeed theText =
    let theChunks = textToChunks size theText
        processFunc = compressH compressHStep extendWStep kSeed
        theGraph = foldl processFunc hSeed theChunks
        results = map bToInt theGraph
        printSize = quot size 32
    in  concatMap (printf ("%0" ++ show printSize ++ "x")) results

