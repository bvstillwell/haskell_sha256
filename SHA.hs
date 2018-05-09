module SHA where

import           Mini
import           Seed
import           SHAHelper

sha :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> String -> String
sha size c1 c2 e1 e2 input =
    let
        mhSeed = map (take (quot size 8)) hSeed
        mkSeed = map (take (quot size 8)) (take (quot size 4) kSeed)
        acompressHStep = SHAHelper.compressHStep (compressHBase c1) (compressHBase c2)
        aextendWStep = SHAHelper.extendWStep (extendWBase e1) (extendWBase e2)
    in
        textToSha acompressHStep aextendWStep size mkSeed mhSeed input

-- sha32 = sha 32 (0, 0, 0) (0, 0, 0) (0, 0, 0) (0, 0, 0)
-- sha64 = sha 64 (0, 0, 0) (0, 0, 0) (0, 0, 0) (0, 0, 0)
-- sha128 = sha 128 (0, 0, 0) (0, 0, 0) (0, 0, 0) (0, 0, 0)
sha256 = sha 256 (2, 13, 22) (6, 11, 25) (7, 18, 3) (17, 19, 10)
sha512 = sha 512 (28, 34, 39) (14, 18, 41) (1, 8, 7) (19, 61, 6)
sha32 = sha 32 (0, 0, 0) (0, 0, 0) (0, 0, 0) (0, 0, 0)
