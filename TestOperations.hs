import           BOperations
import           Test.HUnit

testsBXorO = [
        O ~=? bSXor O O,
        X ~=? bSXor O X,
        V 1 N ~=? bSXor O (V 1 N),
        Bx O O ~=? bSXor O (Bx O O),
        Ba O O ~=? bSXor O (Ba O O),
        O ~=? bSXor O O ,
        X ~=? bSXor X O ,
        V 1 N ~=? bSXor (V 1 N) O ,
        Bx O O ~=? bSXor (Bx O O) O ,
        Ba O O ~=? bSXor (Ba O O) O
    ]

testsBXorTree = [
        "bSXor Order 1" ~: Bx (V 1 N) (V 2 N) ~=? bSXor (V 1 N) (V 2 N),
        "bSXor Order 2" ~: Bx (V 1 N) (V 2 N) ~=? bSXor (V 2 N) (V 1 N),

        "bSXor O ignore" ~: V 2 I ~=? bSXor O (bSXor X (V 2 N)),
        "bSXor X Cancel 1" ~: V 2 N ~=? bSXor X (bSXor X (V 2 N)),
        "bSXor X Cancel 2" ~: V 2 N ~=? bSXor (bSXor X (V 2 N)) X,
        "bSXor Cancel 3" ~: O ~=? bSXor (bSXor X (V 2 N)) (bSXor X (V 2 N)),

        "bSXor Bx Bx Cancel 1" ~: Bx (V 1 N) (V 4 N) ~=? bSXor (Bx (V 1 N) (V 3 N)) (Bx (V 3 N) (V 4 N)),
        "bSXor Bx Bx Cancel 2" ~: Bx (V 1 N) (V 4 N) ~=? bSXor (Bx (V 3 N) (V 4 N)) (Bx (V 1 N) (V 3 N)),

        "bSXor Bx Bx Order 1" ~: Bx (V 1 N) (Bx (V 2 N) (Bx (V 3 N) (V 4 N))) ~=? bSXor (Bx (V 1 N) (V 2 N)) (Bx (V 3 N) (V 4 N)),

        "bSXor Bx Bx Order 1" ~: Bx (V 4 N) (V 10 N) ~=? bSXor (Bx (V 10 N) (V 13 N)) (Bx (V 13 N) (V 4 N)),
        Bx (V 4 N) (V 13 N) ~=? bSXor (V 13 N) (V 4 N),

        Ba (V 1 N) (V 2 I) ~=? bSXor (V 1 N) (Ba (V 1 N) (V 2 N))
    ]


testsBAndTree = [
        "bSAnd Order 1" ~: Ba (V 1 N) (V 2 N) ~=? bSAnd (V 1 N) (V 2 N),
        "bSAnd Order 1" ~: Ba (V 1 N) (V 2 N) ~=? bSAnd (V 2 N) (V 1 N),

        -- From here we are ordered
        "bSAnd O cancels" ~: O ~=? bSAnd O (Ba X (V 2 N)),
        "bSAnd X is cancelled 1" ~: V 2 N ~=? bSAnd X (V 2 N),

        "bSAnd X is cancelled 2" ~: V 2 N ~=? bSAnd (V 2 N) X,
        "bSAnd Same cancels one" ~: V 2 N ~=? bSAnd (V 2 N) (V 2 N),

        -- Ba (V 11 N) ~=? bSAnd (Ba (V 11 N) (bSXor X (V 11 N))) (bSXor X (V 11 N)),
        "bSAnd reorder tree" ~: Ba (V 1 N) (Ba (V 2 N) (Ba (V 3 N) (V 4 N))) ~=? bSAnd (Ba (V 1 N) (V 2 N)) (Ba (V 3 N) (V 4 N)),
        "bSAnd reorder tree" ~: Ba (V 1 N) (Ba (V 2 N) (Ba (V 3 N) (V 4 N))) ~=? bSAnd (Ba (V 2 N) (V 1 N)) (Ba (V 3 N) (V 4 N)),
        "bSAnd reorder tree" ~: Ba (V 1 N) (Ba (V 2 N) (Ba (V 3 N) (V 4 N))) ~=? bSAnd (Ba (V 1 N) (V 3 N)) (Ba (V 2 N) (V 4 N)),
        "bSAnd reorder tree" ~: Ba (V 1 N) (Ba (V 2 N) (Ba (V 3 N) (V 4 N))) ~=? bSAnd (Ba (V 3 N) (V 4 N)) (Ba (V 1 N) (V 2 N))
    ]


results = runTestTT $ test $ testsBXorO ++ testsBXorTree ++ testsBAndTree

theList = [O, X,
        V 1 N, V 2 N, V 3 N,
        Bx X (V 1 N), Bx (V 1 N) (V 2 N), Bx (V 3 N) (V 4 N), Bx (V 2 N) (V 4 N),
        Ba (V 1 N) (V 2 N), Ba (V 3 N) (V 4 N), Ba (V 2 N) (V 4 N)
    ]

combined = [(a, b) | a <- theList, b <- theList]
combinedOutput = map (\(a, b) -> show (bSXor a b) ++ " ~=? bSXor " ++ show a ++ " " ++ show b ++ ",\n") combined
-- combinedOutput = map (\(a, b) -> "bSXor " ++ show a ++ " " ++ show b ++ "\n") combined

main1 = writeFile "TestAuto1.txt" (foldl1 (++) (map (\a -> show a ++ "\n") combined))
main2 = writeFile "TestAuto2.txt" (foldl1 (++) combinedOutput)
