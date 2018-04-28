import           Debug.Trace
import           Mini
import           SHAHelper
import           Test.QuickCheck

instance Arbitrary B where
    arbitrary = do
        r <- choose (1, 10) :: Gen Int
        xs <- sequence [ arbitrary | _ <- [1..3] ]
        case r of n
                    | n == 1 -> return O
                    | n == 2 -> return X
                    | n == 3 -> return (Ba xs)
                    | n == 4 -> return (Bx xs)
                    | otherwise -> return (V r)


bRand = generate arbitrary :: IO B
bRandN n = sequence [ bRand | _ <- [1..n] ]

bXReverse a b = bSXor b a == bSXor b a






