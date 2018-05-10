module Graph where

import           BOperations
import           SHAHelper


genV = map V [0..63]
p = extendWBase (0, 1, 0) $ take 3 genV
