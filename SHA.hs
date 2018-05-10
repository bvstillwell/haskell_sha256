module SHA where

import           BOperations
import           Convert
import           Seed
import           SHAHelper
import           Text.Printf

sha :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> String -> String
sha size s0Rot s1Rot s0RotW s1RotW input =
    let
        fourth = quot size 4
        eighth = quot size 8
        thirtySecond = quot size 32

        -- Create the correct seed size
        mhSeed = map (take eighth) hSeed
        mkSeed = map (take eighth) (take fourth kSeed)

        -- Create the processing  function
        processFunc = compressHLoop s0Rot s1Rot s0RotW s1RotW mkSeed

        -- Convert string into chunks
        theChunks = textToChunks size input

        -- Loop through the chunks
        theGraph = foldl processFunc mhSeed theChunks

        results = map bToInt theGraph
    in
        concatMap (printf ("%0" ++ show thirtySecond ++ "x")) results

sha32 = sha 32 (0, 0, 0) (0, 0, 0) (0, 0, 0) (0, 0, 0)
sha64 = sha 64 (0, 0, 0) (0, 0, 0) (0, 0, 0) (0, 0, 0)
sha128 = sha 128 (0, 0, 0) (0, 0, 0) (0, 0, 0) (0, 0, 0)
sha256 = sha 256 (2, 13, 22) (6, 11, 25) (7, 18, 3) (17, 19, 10)
sha512 = sha 512 (28, 34, 39) (14, 18, 41) (1, 8, 7) (19, 61, 6)
