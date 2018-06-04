import           BOperations
import           Test.HUnit

testsBXorO = [
        O ~=? bSXor O O,
        X ~=? bSXor O X,
        V 1 ~=? bSXor O (V 1),
        Bx O O ~=? bSXor O (Bx O O),
        Ba O O ~=? bSXor O (Ba O O),
        O ~=? bSXor O O ,
        X ~=? bSXor X O ,
        V 1 ~=? bSXor (V 1) O ,
        Bx O O ~=? bSXor (Bx O O) O ,
        Ba O O ~=? bSXor (Ba O O) O
    ]

testsBXorTree = [
        "bSXor Order 1" ~: Bx (V 1) (V 2) ~=? bSXor (V 1) (V 2),
        "bSXor Order 2" ~: Bx (V 1) (V 2) ~=? bSXor (V 2) (V 1),

        "bSXor O ignore" ~: Bx X (V 2) ~=? bSXor O (Bx X (V 2)),
        "bSXor X Cancel 1" ~: V 2 ~=? bSXor X (Bx X (V 2)),
        "bSXor X Cancel 2" ~: V 2 ~=? bSXor (Bx X (V 2)) X,
        "bSXor Cancel 3" ~: O ~=? bSXor (Bx X (V 2)) (Bx X (V 2)),

        "bSXor Bx Bx Cancel 1" ~: Bx (V 1) (V 4) ~=? bSXor (Bx (V 1) (V 3)) (Bx (V 3) (V 4)),
        "bSXor Bx Bx Cancel 2" ~: Bx (V 1) (V 4) ~=? bSXor (Bx (V 3) (V 4)) (Bx (V 1) (V 3)),

        "bSXor Bx Bx Order 1" ~: Bx (V 1) (Bx (V 2) (Bx (V 3) (V 4))) ~=? bSXor (Bx (V 1) (V 2)) (Bx (V 3) (V 4)),

        "bSXor Bx Bx Order 1" ~: Bx (V 4) (V 10) ~=? bSXor (Bx (V 10) (V 13)) (Bx (V 13) (V 4)),
        Bx (V 4) (V 13) ~=? bSXor (V 13) (V 4)
    ]


testsBAndTree = [
        "bSAnd Order 1" ~: Ba (V 1) (V 2) ~=? bSAnd (V 1) (V 2),
        "bSAnd Order 1" ~: Ba (V 1) (V 2) ~=? bSAnd (V 2) (V 1),

        -- From here we are ordered
        "bSAnd O cancels" ~: O ~=? bSAnd O (Ba X (V 2)),
        "bSAnd X is cancelled 1" ~: V 2 ~=? bSAnd X (V 2),

        "bSAnd X is cancelled 2" ~: V 2 ~=? bSAnd (V 2) X,
        "bSAnd Same cancels one" ~: V 2 ~=? bSAnd (V 2) (V 2),

        -- Ba (V 11) ~=? bSAnd (Ba (V 11) (Bx X (V 11))) (Bx X (V 11)),
        "bSAnd reorder tree" ~: Ba (V 1) (Ba (V 2) (Ba (V 3) (V 4))) ~=? bSAnd (Ba (V 1) (V 2)) (Ba (V 3) (V 4)),
        "bSAnd reorder tree" ~: Ba (V 1) (Ba (V 2) (Ba (V 3) (V 4))) ~=? bSAnd (Ba (V 2) (V 1)) (Ba (V 3) (V 4)),
        "bSAnd reorder tree" ~: Ba (V 1) (Ba (V 2) (Ba (V 3) (V 4))) ~=? bSAnd (Ba (V 1) (V 3)) (Ba (V 2) (V 4)),
        "bSAnd reorder tree" ~: Ba (V 1) (Ba (V 2) (Ba (V 3) (V 4))) ~=? bSAnd (Ba (V 3) (V 4)) (Ba (V 1) (V 2))
    ]


results = runTestTT $ test $ testsBXorO ++ testsBXorTree ++ testsBAndTree

theList = [O, X, V 1, V 2, V 3,
        Bx X (V 1), Bx (V 1) (V 2), Bx (V 3) (V 4), Bx (V 2) (V 4),
        Ba (V 1) (V 2), Ba (V 3) (V 4), Ba (V 2) (V 4)
    ]

combined = [(a, b) | a <- theList, b <- theList]
combinedOutput = map (\(a, b) -> show (bSXor a b) ++ " ~=? bSXor " ++ show a ++ " " ++ show b ++ ",\n") combined
-- combinedOutput = map (\(a, b) -> "bSXor " ++ show a ++ " " ++ show b ++ "\n") combined

main1 = writeFile "TestAuto1.txt" (foldl1 (++) (map (\a -> show a ++ "\n") combined))
main2 = writeFile "TestAuto2.txt" (foldl1 (++) combinedOutput)
