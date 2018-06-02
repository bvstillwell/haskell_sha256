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

        Bx (V 4) (V 10) ~=? bSXor (Bx (V 10) (V 13)) (Bx (V 13) (V 4)),
        Bx (V 4) (V 13) ~=? bSXor (V 13) (V 4)
    ]


testsBAndTree = [
        Ba (V 1) (V 2) ~=? bSAnd (V 1) (V 2),
        Ba (V 1) (V 2) ~=? bSAnd (V 2) (V 1),

        O ~=? bSAnd O (Ba X (V 2)),
        V 2 ~=? bSAnd X (V 2),
        V 2 ~=? bSAnd (V 2) X,
        V 2 ~=? bSAnd X (Ba X (V 2)),
        V 2 ~=? bSAnd (Ba X (V 2)) X,
        Ba X (V 2) ~=? bSAnd (Ba X (V 2)) (Ba X (V 2)),

        Ba (V 1) (Ba (V 3) (V 4)) ~=? bSAnd (Ba (V 1) (V 3)) (Ba (V 3) (V 4)),
        Ba (V 1) (Ba (V 3) (V 4)) ~=? bSAnd (Ba (V 3) (V 4)) (Ba (V 1) (V 3)),
        Ba (V 1) (Ba (V 2) (Ba (V 3) (V 4))) ~=? bSAnd (Ba (V 1) (V 2)) (Ba (V 3) (V 4)),

        Ba (V 4) (Ba (V 10) (V 13)) ~=? bSAnd (Ba (V 10) (V 13)) (Ba (V 13) (V 4)),
        Ba (V 4) (V 13) ~=? bSAnd (V 13) (V 4)
    ]


results = runTestTT $ test $ testsBXorO ++ testsBXorTree ++ testsBAndTree
