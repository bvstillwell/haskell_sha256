import           BOperations
import           Convert
import           Data.List.Split
import           Seed
import           SHA
import           SHAHelper

size = 32
b = textToChunks "brad" size
theChunks1 = [V 1 N, X, X, O] : tail (head b)
theChunks2 = [V 1 N, V 2 N, X, O] : tail (head b)
theChunks3 = [V 1 N, V 2 N, V 3 N, O] : tail (head b)
-- theChunks = head b
(s0Rot, s1Rot, s0RotW, s1RotW) = ((0, 0, 0), (0, 0, 0), (0, 0, 0), (0, 0, 0))



-- Infinite func bSAdd X X (Bx (V 1 N) (Bx (V 1 N) X))
--
--
fourth = quot size 4
eighth = quot size 8

hInit = map (take eighth) hSeed -- Create the correct list size
kInit = map (take eighth) (take fourth kSeed) -- Create the correct list size

compressFunc = compressChunk (compressHStep s0Rot s1Rot) (createKWVector s0RotW s1RotW kInit)
result1 = compressFunc hInit theChunks1
result2 = compressFunc hInit theChunks2
result3 = compressFunc hInit theChunks3
