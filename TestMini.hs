{-# LANGUAGE TemplateHaskell #-}
import           Debug.Trace
import           Mini
import           SHAHelper
import           Test.QuickCheck

-- This class creates random B. Try it out with bRand
instance Arbitrary B where
    arbitrary = do
        r <- choose (1, 10) :: Gen Int
        n <- choose (0, 3) :: Gen Int
        xs <- sequence [ arbitrary | _ <- [1..n] ]
        case r of n
                    | n == 1 -> return O
                    | n == 2 -> return X
                    | n == 3 -> return (Ba xs)
                    | n == 4 -> return (Bx xs)
                    | otherwise -> return (V r)


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

-- prop_bNot = fbThrice bSNot
--------------------------
return []
runTests :: IO Bool
runTests = $forAllProperties $
  quickCheckWithResult (stdArgs {maxSuccess = 1000})
