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

compressHStep ::  (Int, Int, Int) ->  (Int, Int, Int) ->  [[B]] -> [B] -> [B] -> [[B]]
compressHStep s0Rot s1Rot [a, b, c, d, e, f, g, h] k w =
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

compressHLoop :: (Int, Int, Int) ->  (Int, Int, Int) -> (Int, Int, Int) ->  (Int, Int, Int) -> [[B]] -> [[B]] -> [[B]] -> [[B]]
compressHLoop s0Rot s1Rot s0Rotw s1Rotw kSeed h w =
    let
        extendWLoop' = extendWLoop s0Rotw s1Rotw (length kSeed) w
        zipKSeedW = zip kSeed extendWLoop'
        hResult = foldl (\hAcc (k, w) -> compressHStep s0Rot s1Rot hAcc k w) h zipKSeedW
    in
        zipWith bAddNoCarry h hResult

