import           BOperations
import           Convert
import           Data.List.Split
import           SHA
import           SHAHelper
import           Test.HUnit

-- p = chunksOf 4 [V a | a <- [1..(16*4)]]

b = textToChunks "brad" 32

c = [V n | n <- [0 .. 4]] : tail (head b)

result = extendWStep (0, 0, 0) (0, 0, 0) c

-- result = shaChunks (0, 0, 0) (0, 0, 0) (0, 0, 0) (0, 0, 0) 32 p
