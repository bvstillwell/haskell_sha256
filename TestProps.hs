{-# LANGUAGE TemplateHaskell #-}
import           BOperations
import           Convert
import           Debug.Trace
import           SHAHelper
import           Test.QuickCheck
import           Test.QuickCheck.Gen

-- This class creates random B. Try it out with bRand
instance Arbitrary B where
    arbitrary =
        do
        n <- choose (1, 5) :: Gen Int
        r1 <- choose (1, 3) :: Gen Int
        r2 <- choose (1, 3) :: Gen Int
        s1 <- elements [N, I]
        s2 <- elements [N, I]
        case n of n
                    | n == 1 -> return O
                    | n == 2 -> return X
                    | n == 3 -> return (V r1 s1)
                    | n == 4 -> return (bSXor (V r1 s1) (V r2 s2))
                    | n == 5 -> return (bSAnd (V r1 s1) (V r2 s2))
                    -- | otherwise -> return (V r s)


bRand = generate arbitrary :: IO B --Create a random B
bRandN n =  [ bRand | _ <- [1..n] ] --Create a list of B

fbReverse f a b = f b a == f a b -- reverse args equal
fbReverseEq f a b e = f b a == e && f b a == e -- reverse args equal something
fbTwice f a = f (f a) == f a
fbThrice f a = f (f $ f a) == f a

prop_bXReverse = fbReverse bSXor
prop_bXNop a = fbReverseEq bSXor a O a
prop_bXClear a = bSXor a a == O

prop_bAReverse = fbReverse bSAnd
prop_bAClear a = fbReverseEq bSAnd O a O
prop_bANop a = fbReverseEq bSAnd X a a


-- prop_bAddReverse1 a b c = bSAdd a b c == bSAdd b a c

-- prop_bNot = fbThrice bSNot
--------------------------
return []
result :: IO Bool
result = $forAllProperties $
  quickCheckWithResult (stdArgs {maxSuccess = 1000})
