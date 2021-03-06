import           Data.List
import           SHA        (sha256, sha512)
import           Test.HUnit

testCases = [
    ("abc",
        ["ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
        -- "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7",
        "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"]
    ),
    ("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu",
        ["cf5b16a778af8380036ce59e7b0492370b249b11e8f07a51afac45037afee9d1",
        -- "09330c33f71147e83d192fc782cd1b4753111b173b3b05d22fa08086e3b0f712fcc7c71a557e2db966c3e9fa91746039",
        "8e959b75dae313da8cf4f72814fc143f8f7779c6eb9f7fa17299aeadb6889018501d289e4900f7e4331b99dec4b5433ac7d329eeb6dd26545e96e55b874be909"]
    )]

methods = [
    sha256,
    -- SHA384.sha384,
    sha512]

sizes = [
    256,
    -- 384,
    512]

tests2 = concat [zip4 sizes methods (replicate (length methods) vector) results | (vector, results) <- testCases]

tests = TestList [TestCase (assertEqual ("Sha" ++ show size ++ ":" ++ vector) result (method vector)) |
    (size, method, vector, result) <- tests2]

results = runTestTT tests
