import           BOperations
import           Convert
import           Data.List.Split
import           Seed
import           SHA
import           SHAHelper

result size count =
    let
        b = textToChunks "brad" size
        (s0Rot, s1Rot, s0RotW, s1RotW) = ((0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0))

        m1 = replicate (eighth - count) O
        m2 = [V a N | a <- [1..count]] ++ m1

        fourth = quot size 4
        eighth = quot size 8

        hInit = map (take eighth) hSeed -- Create the correct list size
        kInit = map (take eighth) (take fourth kSeed) -- Create the correct list size

        compressFunc = compressChunk (compressHStep s0Rot s1Rot) (createKWVector s0RotW s1RotW kInit)
    in compressFunc hInit $ m2 : tail (head b)

result1 = result 32 1
result256 = result 256 2
