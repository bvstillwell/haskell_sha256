module SHA384 where

import           Mini
import           SHA512    (sha512k)
import           SHAHelper

hSeed = map bIntTo64 [
    0xcbbb9d5dc1059ed8,
    0x629a292a367cd507,
    0x9159015a3070dd17,
    0x152fecd8f70e5939,
    0x67332667ffc00b31,
    0x8eb44a8768581511,
    0xdb0c2e0d64f98fa7,
    0x47b5481dbefa4fa4]

sha384 theText = take 96 $ SHA512.sha512k hSeed theText
