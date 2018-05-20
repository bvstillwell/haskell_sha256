module SHA where

import           BOperations
import           Convert
import           Seed
import           SHAHelper
import           Text.Printf

sha :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int) -> Int -> String -> String
sha s0Rot s1Rot s0RotW s1RotW size input =
    let
        thirtySecond = quot size 32
        fourth = quot size 4
        eighth = quot size 8

        hInit = map (take eighth) hSeed -- Create the correct list size
        kInit = map (take eighth) (take fourth kSeed) -- Create the correct list size

        theChunks = textToChunks input size -- Turn the string into chunks
        theGraph = shaChunks s0Rot s1Rot s0RotW s1RotW hInit kInit theChunks -- Create the graph
        results = map bToInt theGraph -- Convert back into a readbale fomat
    in
        concatMap (printf ("%0" ++ show thirtySecond ++ "x")) results

shaChunks s0Rot s1Rot s0RotW s1RotW hInit kInit theChunks =
    let
        compressFunc = compressChunk (compressHStep s0Rot s1Rot) (createKWVector s0RotW s1RotW kInit)
    in
        foldl compressFunc hInit theChunks

sha32 = sha (0, 0, 0) (0, 0, 0) (0, 0, 0) (0, 0, 0) 32
sha64 = sha (0, 0, 0) (0, 0, 0) (0, 0, 0) (0, 0, 0) 64
sha128 = sha (0, 0, 0) (0, 0, 0) (0, 0, 0) (0, 0, 0) 128
sha256 = sha (2, 13, 22) (6, 11, 25) (7, 18, 3) (17, 19, 10) 256
sha512 = sha (28, 34, 39) (14, 18, 41) (1, 8, 7) (19, 61, 6) 512
