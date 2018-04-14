module SHA where

import           Mini
import           Seed
import           SHAHelper

sha :: Int -> String -> String
sha size input =
    let
        mhSeed = map (take (quot size 8)) hSeed
        mkSeed = map (take (quot size 8)) (take (quot size 4) kSeed)
        acompressHStep = SHAHelper.compressHStep (compressHBase 2 13 22) (compressHBase 6 11 25)
        aextendWStep = SHAHelper.extendWStep (extendWBase 7 18 3) (extendWBase 17 19 10)
    in
        textToSha acompressHStep aextendWStep size mkSeed mhSeed input
