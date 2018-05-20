module SHAHelper where

import           BOperations

-- extend W step
extendWCalc :: (Int, Int, Int) -> [B] -> [B]
extendWCalc (a, b, c) x = bRotR a x `bXor` bRotR b x `bXor` bShR c x

extendWStep :: (Int, Int, Int) ->  (Int, Int, Int) -> [[B]] -> [B]
extendWStep s0w s1w ws =
    -- s0 := (w[i-15] rightrotate 7) xor (w[i-15] rightrotate 18) xor (w[i-15] rightshift 3)
    -- s1 := (w[i-2] rightrotate 17) xor (w[i-2] rightrotate 19) xor (w[i-2] rightshift 10)
    -- w[i] := w[i-16] + s0 + w[i-7] + s1
    let
        s0 = extendWCalc s0w
        s1 = extendWCalc s1w
        l = length ws
    in     (ws !! (l-16)) `bAddNoCarry`
        s0 (ws !! (l-15)) `bAddNoCarry`
           (ws !! (l -7)) `bAddNoCarry`
        s1 (ws !! (l -2))

extendWLoop :: (Int, Int, Int) ->  (Int, Int, Int) -> Int -> [[B]] -> [[B]]
extendWLoop s0w s1w steps chunks = foldl (\wAcc _ -> wAcc ++ [extendWStep s0w s1w wAcc]) chunks [1..steps]

-- Compress H func
compressHCalc :: (Int, Int, Int) -> [B] -> [B]
compressHCalc (a, b, c) x = bRotR a x `bXor` bRotR b x `bXor` bRotR c x

compressHStep ::  (Int, Int, Int) ->  (Int, Int, Int) ->  [[B]] -> ([B], [B]) -> [[B]]
compressHStep s0Rot s1Rot [a, b, c, d, e, f, g, h] (k, w) =
    -- S1 := (e rightrotate 6) xor (e rightrotate 11) xor (e rightrotate 25)
    -- ch := (e and f) xor ((not e) and g)
    -- temp1 := h + S1 + ch + k[i] + w[i]
    -- S0 := (a rightrotate 2) xor (a rightrotate 13) xor (a rightrotate 22)
    -- maj := (a and b) xor (a and c) xor (b and c)
    -- temp2 := S0 + maj
    --
    -- h := g
    -- g := f
    -- f := e
    -- e := d + temp1
    -- d := c
    -- c := b
    -- b := a
    -- a := temp1 + temp2
    --
    let
        s1 = compressHCalc s1Rot
        ch = (e `bAnd` f) `bXor` (bNot e `bAnd` g)
        temp1 = h `bAddNoCarry` s1 e `bAddNoCarry` ch `bAddNoCarry` k `bAddNoCarry` w
        s0 = compressHCalc s0Rot
        maj = (a `bAnd` b) `bXor` (a `bAnd` c) `bXor` (b `bAnd` c)
        temp2 = s0 a `bAddNoCarry` maj
    in
        [temp1 `bAddNoCarry` temp2, a, b, c, d `bAddNoCarry` temp1, e, f, g]

createKWVector :: (Int, Int, Int) -> (Int, Int, Int) -> [[B]] -> [[B]] -> [([B], [B])]
-- Return a list of k,w created from a chunk for use in the compressHFunc
createKWVector s0Rotw s1Rotw kInit chunk = zip kInit (extendWLoop s0Rotw s1Rotw (length kInit) chunk)

compressChunk :: ([[B]] -> ([B], [B]) -> [[B]]) -> ([[B]] -> [([B], [B])]) -> [[B]] -> [[B]] -> [[B]]
compressChunk compressHFunc kwVectorFunc hInit chunk =
    let
        kwVector = kwVectorFunc chunk
        hResult = foldl compressHFunc hInit kwVector
    in
        -- Add the compressed chunk to the current hash value:
        -- h0 := h0 + a
        -- h1 := h1 + b
        -- h2 := h2 + c
        -- h3 := h3 + d
        -- h4 := h4 + e
        -- h5 := h5 + f
        -- h6 := h6 + g
        -- h7 := h7 + h
        zipWith bAddNoCarry hInit hResult
