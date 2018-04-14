module SHA512 where

import           Mini
import           Seed
import           SHAHelper

mhSeed = map (take 64) hSeed
mkSeed = map (take 64) (take 80 kSeed)

compressHStep = SHAHelper.compressHStep (compressHBase 28 34 39) (compressHBase 14 18 41)
extendWStep = SHAHelper.extendWStep (extendWBase 1 8 7) (extendWBase 19 61 6)
sha512k = textToSha SHA512.compressHStep SHA512.extendWStep 512 mkSeed
sha512 = sha512k mhSeed
