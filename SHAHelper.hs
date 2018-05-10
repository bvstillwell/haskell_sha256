module SHAHelper where

import           BOperations

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

