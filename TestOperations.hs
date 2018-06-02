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
        Bx (V 1) (V 2) ~=? bSXor (V 1) (V 2),
        Bx (V 1) (V 2) ~=? bSXor (V 2) (V 1),

        Bx X (V 2) ~=? bSXor O (Bx X (V 2)),
        V 2 ~=? bSXor X (Bx X (V 2)),
        V 2 ~=? bSXor (Bx X (V 2)) X,
        O ~=? bSXor (Bx X (V 2)) (Bx X (V 2)),

        Bx (V 1) (V 4) ~=? bSXor (Bx (V 1) (V 3)) (Bx (V 3) (V 4)),
        Bx (V 1) (V 4) ~=? bSXor (Bx (V 3) (V 4)) (Bx (V 1) (V 3)),

        Bx (V 1) (Bx (V 2) (Bx (V 3) (V 4))) ~=? bSXor (Bx (V 1) (V 2)) (Bx (V 3) (V 4)),

        -- Bx (Bx (V 1) (V 2)) (Bx (V 3) (V 4)) ~=? bSXor (Bx (V 1) (V 2)) (Bx (V 4) (V 3)),
        -- Bx (Bx (V 1) (V 2)) (Bx (V 3) (V 4)) ~=? bSXor (Bx (V 1) (V 3)) (Bx (V 4) (V 1)),
        -- Bx (Bx (V 1) (V 2)) (Bx (V 3) (V 4)) ~=? bSXor (Bx (V 3) (V 2)) (Bx (V 4) (V 1)),
        -- Bx (Bx (V 1) (V 2)) (Bx (V 3) (V 4)) ~=? bSXor (Bx (V 4) (V 3)) (Bx (V 2) (V 1)),

        Bx (V 4) (V 10) ~=? bSXor (Bx (V 10) (V 13)) (Bx (V 13) (V 4)),
        Bx (V 4) (V 13) ~=? bSXor (V 13) (V 4)
    ]


results = runTestTT $ test $ testsBXorO ++ testsBXorTree
