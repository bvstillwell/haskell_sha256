import           Data.Char       (intToDigit, ord)
import           Data.List.Split
import           Mini
import           Numeric         (showHex)

hSeed = map bIntTo32 [
    0x6a09e667,
    0xbb67ae85,
    0x3c6ef372,
    0xa54ff53a,
    0x510e527f,
    0x9b05688c,
    0x1f83d9ab,
    0x5be0cd19]

kSeed = map bIntTo32 [
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]

textToChunks :: String -> [[[B]]]
textToChunks theText = let
    -- Pre-processing:
    -- begin with the original message of length L bits
    -- append a single '1' bit
    -- append K '0' bits, where K is the minimum number >= 0 such that L + 1 + K + 64 is a multiple of 512
    -- append L as a 64-bit big-endian integer, making the total post-processed length a multiple of 512 bits
    msg = concatMap (bIntTo8.ord) theText
    addZeros = 512 - mod (length msg + 1 + 64) 512
    -- Create the initial msg
    msg2 = msg ++ [X] ++ replicate addZeros O ++ bIntTo64 (length msg)
    -- Create 512byte chunks.
    chunks512 = chunksOf 512 msg2
        in map (chunksOf 32) chunks512

extendWStep :: [[B]] -> [B]
extendWStep ws =
    let
        l = length ws
        wn16 = ws !! (l-16)
        wn15 = ws !! (l-15)
        wn7 = ws !! (l-7)
        wn2 = ws !! (l-2)
        s0 = bRotR wn15 7 `bXor` bRotR wn15 18 `bXor` bShR wn15 3
        s1 = bRotR wn2 17 `bXor` bRotR wn2 19 `bXor` bShR wn2 10
    in
        wn16 `bAddNoCarry` s0 `bAddNoCarry` wn7 `bAddNoCarry` s1

extendW :: [[B]] -> [[B]]
extendW chunks = foldl (\wAcc _ -> wAcc ++ [extendWStep wAcc]) chunks [1..48]

compressHStep :: [[B]] -> [B] -> [B] -> [[B]]
compressHStep [a, b, c, d, e, f, g, h] k w =
    let
        s1 = bRotR e 6 `bXor` bRotR e 11 `bXor` bRotR e 25
        ch = (e `bAnd` f) `bXor` (bNot e `bAnd` g)
        temp1 = h `bAddNoCarry` s1 `bAddNoCarry` ch `bAddNoCarry` k `bAddNoCarry` w
        s0 = bRotR a 2 `bXor` bRotR a 13 `bXor` bRotR a 22
        maj = (a `bAnd` b) `bXor` (a `bAnd` c) `bXor` (b `bAnd` c)
        temp2 = s0 `bAddNoCarry` maj
    in
        [temp1 `bAddNoCarry` temp2, a, b, c, d `bAddNoCarry` temp1, e, f, g]

compressH :: [[B]] -> [[B]] -> [[B]]
compressH h w =
    let hResult = foldl (\hAcc (k, w) -> compressHStep hAcc k w) h (zip kSeed (extendW w))
    in zipWith bAddNoCarry h hResult

textToSha256 :: String -> String
textToSha256 theText =
    let results = map bToInt $ foldl compressH hSeed (textToChunks theText)
    in concatMap (`showHex` "") results

output = textToSha256 ""

