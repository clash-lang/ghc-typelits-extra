{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ShouldError (tests) where

import Data.String.Interpolate (i)
import ShouldError.Tasty (assertCompileError)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase)

tests :: TestTree
tests = testGroup "ShouldError"
    [ test1
    , test2
    , test3
    , test4
    , test5
    , test6
    , test7
    , test8
    , test9
    , test10
    , test11
    , test12
    , test13
    , test14
    , test15
    , test16
    , test17
    , test18
    , test19
    , test20
    , test21
    , test22
    , test23
    , test24
    , test25
    , test26
    , test27
    , test28
    ]

preamble :: String
preamble = [i|
import Data.Proxy
import GHC.TypeLits
import GHC.TypeLits.Extra
|] <> "\n"

source1 :: String
source1 = preamble <> [i|
test :: Proxy (GCD 6 8) -> Proxy 4
test = id
|]

expected1 :: [String]
expected1 =
  ["Proxy (GCD 6 8) -> Proxy 4"
  ,"Proxy 4 -> Proxy 4"
  ]

test1 :: TestTree
test1 = testCase "GCD 6 8 /~ 4" $ assertCompileError source1 expected1

source2 :: String
source2 = preamble <> [i|
test :: Proxy ((GCD 6 8) + x) -> Proxy (x + (GCD 6 9))
test = id
|]

expected2 :: [String]
expected2 =
#if __GLASGOW_HASKELL__ >= 904
  ["Proxy (GCD 6 8 + x) -> Proxy (x + GCD 6 9)"
  ,"Proxy (2 + x) -> Proxy (2 + x)"
  ]
#elif __GLASGOW_HASKELL__ >= 900
  ["Proxy (GCD 6 8 + x) -> Proxy (x + GCD 6 9)"
  ,"Proxy (GCD 6 8 + x) -> Proxy (GCD 6 8 + x)"
  ]
#else
  ["Expected type: Proxy (GCD 6 8 + x) -> Proxy (x + GCD 6 9)"
  ,"Actual type: Proxy (x + GCD 6 9) -> Proxy (x + GCD 6 9)"
  ]
#endif

test2 :: TestTree
test2 = testCase "GCD 6 8 + x /~ x + GCD 9 6" $ assertCompileError source2 expected2

source3 :: String
source3 = preamble <> [i|
test :: Proxy (CLog 3 10) -> Proxy 2
test = id
|]

expected3 :: [String]
expected3 =
  ["Proxy (CLog 3 10) -> Proxy 2"
  ,"Proxy 2 -> Proxy 2"
  ]

test3 :: TestTree
test3 = testCase "CLog 3 10 /~ 2" $ assertCompileError source3 expected3

source4 :: String
source4 = preamble <> [i|
test :: Proxy ((CLog 3 10) + x) -> Proxy (x + (CLog 2 9))
test = id
|]

expected4 :: [String]
expected4 =
#if __GLASGOW_HASKELL__ >= 904
  ["Proxy (CLog 3 10 + x) -> Proxy (x + CLog 2 9)"
  ,"Proxy (3 + x) -> Proxy (3 + x)"
  ]
#elif __GLASGOW_HASKELL__ >= 900
  ["Proxy (CLog 3 10 + x) -> Proxy (x + CLog 2 9)"
  ,"Proxy (CLog 3 10 + x) -> Proxy (CLog 3 10 + x)"
  ]
#else
  ["Proxy (CLog 3 10 + x) -> Proxy (x + CLog 2 9)"
  ,"Proxy (x + CLog 2 9) -> Proxy (x + CLog 2 9)"
  ]
#endif

test4 :: TestTree
test4 = testCase "CLog 3 10 + x /~ x + CLog 2 10" $ assertCompileError source4 expected4

source5 :: String
source5 = preamble <> [i|
test :: Proxy (CLog 0 4) -> Proxy 100
test = id
|]

expected5 :: [String]
expected5 =
  ["Proxy (CLog 0 4) -> Proxy 100"
  ,"Proxy 100 -> Proxy 100"
  ]

test5 :: TestTree
test5 = testCase "CLog 0 4 /~ 100" $ assertCompileError source5 expected5

source6 :: String
source6 = preamble <> [i|
test :: Proxy (CLog 1 4) -> Proxy 100
test = id
|]

expected6 :: [String]
expected6 =
  ["Proxy (CLog 1 4) -> Proxy 100"
  ,"Proxy 100 -> Proxy 100"
  ]

test6 :: TestTree
test6 = testCase "CLog 1 4 /~ 100" $ assertCompileError source6 expected6

source7 :: String
source7 = preamble <> [i|
test :: Proxy (CLog 4 0) -> Proxy 0
test = id
|]

expected7 :: [String]
expected7 =
  ["Proxy (CLog 4 0) -> Proxy 0"
  ,"Proxy 0 -> Proxy 0"
  ]

test7 :: TestTree
test7 = testCase "CLog 4 0 /~ 0" $ assertCompileError source7 expected7

source8 :: String
source8 = preamble <> [i|
test :: Proxy (CLog 1 (1^y)) -> Proxy y
test = id
|]

expected8 :: [String]
expected8 =
  ["Proxy (CLog 1 (1 ^ y)) -> Proxy y"
  ,"Proxy y -> Proxy y"
  ]

test8 :: TestTree
test8 = testCase "CLog 1 (1^y) /~ y" $ assertCompileError source8 expected8

source9 :: String
source9 = preamble <> [i|
test :: Proxy (CLog 0 (0^y)) -> Proxy y
test = id
|]

expected9 :: [String]
expected9 =
  ["Proxy (CLog 0 (0 ^ y)) -> Proxy y"
  ,"Proxy y -> Proxy y"
  ]

test9 :: TestTree
test9 = testCase "CLog 0 (0^y) /~ y" $ assertCompileError source9 expected9

source10 :: String
source10 = [i|
{-# LANGUAGE TypeApplications #-}
|] <> preamble <> [i|
test :: Integer
test = natVal (Proxy :: Proxy (CLog 1 4))
|]

expected10 :: [String]
expected10 =
#if __GLASGOW_HASKELL__ >= 904
  ["Cannot satisfy: 2 <= 1"]
#else
  ["Couldn't match type ''False' with ''True'"]
#endif

test10 :: TestTree
test10 = testCase "No instance (KnownNat (CLog 1 4))" $ assertCompileError source10 expected10

source11 :: String
source11 = [i|
{-# LANGUAGE TypeApplications #-}
|] <> preamble <> [i|
test :: Integer
test = natVal (Proxy :: Proxy ((CLog 4 4) - (CLog 2 4)))
|]

expected11 :: [String]
expected11 =
#if __GLASGOW_HASKELL__ >= 904
  ["Cannot satisfy: 2 <= 1"]
#else
  ["Couldn't match type ''False` with ''True'"]
#endif

test11 :: TestTree
test11 = testCase "No instance (KnownNat (CLog 4 4 - CLog 2 4))" $ assertCompileError source11 expected11

source12 :: String
source12 = preamble <> [i|
test :: Proxy (Div 4 0) -> Proxy 4
test = id
|]

expected12 :: [String]
expected12 =
  ["Proxy (Div 4 0) -> Proxy 4"
  ,"Proxy 4 -> Proxy 4"
  ]

test12 :: TestTree
test12 = testCase "Div 4 0 /~ 4" $ assertCompileError source12 expected12

source13 :: String
source13 = preamble <> [i|
test :: Proxy (Mod 4 0) -> Proxy 4
test = id
|]

expected13 :: [String]
expected13 =
  ["Proxy (Mod 4 0) -> Proxy 4"
  ,"Proxy 4 -> Proxy 4"
  ]

test13 :: TestTree
test13 = testCase "Mod 4 0 /~ 4" $ assertCompileError source13 expected13

source14 :: String
source14 = preamble <> [i|
test :: Proxy (FLog 0 4) -> Proxy 100
test = id
|]

expected14 :: [String]
expected14 =
  ["Proxy (FLog 0 4) -> Proxy 100"
  ,"Proxy 100 -> Proxy 100"
  ]

test14 :: TestTree
test14 = testCase "FLog 0 4 /~ 100" $ assertCompileError source14 expected14

source15 :: String
source15 = preamble <> [i|
test :: Proxy (FLog 1 4) -> Proxy 100
test = id
|]

expected15 :: [String]
expected15 =
  ["Proxy (FLog 1 4) -> Proxy 100"
  ,"Proxy 100 -> Proxy 100"
  ]

test15 :: TestTree
test15 = testCase "FLog 1 4 /~ 100" $ assertCompileError source15 expected15

source16 :: String
source16 = preamble <> [i|
test :: Proxy (FLog 4 0) -> Proxy 0
test = id
|]

expected16 :: [String]
expected16 =
  ["Proxy (FLog 4 0) -> Proxy 0"
  ,"Proxy 0 -> Proxy 0"
  ]

test16 :: TestTree
test16 = testCase "FLog 4 0 /~ 0" $ assertCompileError source16 expected16

source17 :: String
source17 = preamble <> [i|
test :: Proxy (LCM 6 8) -> Proxy 48
test = id
|]

expected17 :: [String]
expected17 =
  ["Proxy (LCM 6 8) -> Proxy 48"
  ,"Proxy 48 -> Proxy 48"
  ]

test17 :: TestTree
test17 = testCase "LCM 6 8 /~ 48" $ assertCompileError source17 expected17

source18 :: String
source18 = preamble <> [i|
test :: Proxy ((LCM 6 8) + x) -> Proxy (x + (LCM 6 9))
test = id
|]

expected18 :: [String]
expected18 =
#if __GLASGOW_HASKELL__ >= 904
  ["Proxy (LCM 6 8 + x) -> Proxy (x + LCM 6 9)"
  ,"Proxy (24 + x) -> Proxy (24 + x)"
  ]
#elif __GLASGOW_HASKELL__ >= 900
  ["Proxy (LCM 6 8 + x) -> Proxy (x + LCM 6 9)"
  ,"Proxy (LCM 6 8 + x) -> Proxy (LCM 6 8 + x)"
  ]
#else
  ["Proxy (LCM 6 8 + x) -> Proxy (x + LCM 6 9)"
  ,"Proxy (x + LCM 6 9) -> Proxy (x + LCM 6 9)"
  ]
#endif

test18 :: TestTree
test18 = testCase "LCM 6 8 + x /~ x + LCM 6 9" $ assertCompileError source18 expected18

source19 :: String
source19 = [i|
{-# LANGUAGE TypeApplications #-}
|] <> preamble <> [i|
test :: Integer
test = natVal (Proxy :: Proxy (Log 3 0))
|]

expected19 :: [String]
expected19 =
#if __GLASGOW_HASKELL__ >= 900
  ["Couldn't match type: FLog 3 0"
  ,"               with: CLog 3 0"]
#else
  ["Couldn't match type 'FLog 3 0' with 'CLog 3 0'"]
#endif

test19 :: TestTree
test19 = testCase "No instance (KnownNat (Log 3 0))" $ assertCompileError source19 expected19

source20 :: String
source20 = [i|
{-# LANGUAGE TypeApplications #-}
|] <> preamble <> [i|
test :: Integer
test = natVal (Proxy :: Proxy (Log 3 10))
|]

expected20 :: [String]
expected20 =
#if __GLASGOW_HASKELL__ >= 904
  ["Couldn't match type '2' with '3'"]
#elif __GLASGOW_HASKELL__ >= 900
  ["Couldn't match type: FLog 3 10"
  ,"               with: CLog 3 10"]
#else
  ["Couldn't match type 'FLog 3 10' with 'CLog 3 10'"]
#endif

test20 :: TestTree
test20 = testCase "No instance (KnownNat (Log 3 10))" $ assertCompileError source20 expected20

source21 :: String
source21 = preamble <> [i|
test :: Proxy a -> Proxy b -> Proxy (Min a (a * b)) -> Proxy a
test _ _ = id
|]

expected21 :: [String]
expected21 =
  ["Proxy (Min a (a * b)) -> Proxy a"
  ,"Proxy a -> Proxy a"
  ]

test21 :: TestTree
test21 = testCase "Min a (a*b) /~ a" $ assertCompileError source21 expected21

source22 :: String
source22 = preamble <> [i|
test :: Proxy a -> Proxy b -> Proxy (Max a (a * b)) -> Proxy (a * b)
test _ _ = id
|]

expected22 :: [String]
expected22 =
#if __GLASGOW_HASKELL__ >= 900
  ["Proxy (Max a (a * b)) -> Proxy (a * b)"
  ,"Proxy (Max a (a * b)) -> Proxy (Max a (a * b))"]
#else
  ["Proxy (Max a (a * b)) -> Proxy (a * b)"
  ,"Proxy (a * b) -> Proxy (a * b)"]
#endif

test22 :: TestTree
test22 = testCase "Max a (a*b) /~ (a*b)" $ assertCompileError source22 expected22

source23 :: String
source23 = [i|
{-# LANGUAGE TypeApplications #-}
|] <> preamble <> [i|
test' :: ((1 <=? Div l r) ~ False) => Proxy l -> Proxy r -> ()
test' _ _ = ()

test :: ()
test = test' (Proxy @18) (Proxy @3)
|]

expected23 :: [String]
expected23 =
#if __GLASGOW_HASKELL__ >= 804
  ["Couldn't match type ''True' with ''False'"]
#else
  ["Couldn't match type '1 <=? Div 18 3' with ''False'"]
#endif

test23 :: TestTree
test23 = testCase "(1 <=? Div 18 3) ~ False" $ assertCompileError source23 expected23

source24 :: String
source24 = [i|
{-# LANGUAGE CPP #-}
|] <> preamble <> [i|
#if __GLASGOW_HASKELL__ >= 901
import qualified Data.Type.Ord
#endif

test :: Proxy x -> Proxy y -> Proxy z -> Proxy (z <=? Max x y) -> Proxy True
test _ _ _ = id
|]

expected24 :: [String]
expected24 =
#if __GLASGOW_HASKELL__ >= 902
  ["Couldn't match type 'Data.Type.Ord.OrdCond"
  ,"(CmpNat z (Max x y)) 'True 'True 'False'"
  ,"with ''True'"]
#else
  ["Couldn't match type 'z <=? Max x y' with ''True'"]
#endif

test24 :: TestTree
test24 = testCase "(z <=? Max x y) /~ True" $ assertCompileError source24 expected24

source25 :: String
source25 = [i|
{-# LANGUAGE CPP #-}
|] <> preamble <> [i|
#if __GLASGOW_HASKELL__ >= 901
import qualified Data.Type.Ord
#endif

test :: Proxy x -> Proxy y -> Proxy (x+1 <=? Max x y) -> Proxy True
test _ _ = id
|]

expected25 :: [String]
expected25 =
#if __GLASGOW_HASKELL__ >= 902
  ["Couldn't match type 'Data.Type.Ord.OrdCond"
  ,"(CmpNat (x + 1) (Max x y)) 'True 'True 'False'"
  ,"with ''True'"]
#else
  ["Couldn't match type '(x + 1) <=? Max x y' with ''True'"]
#endif

test25 :: TestTree
test25 = testCase "(x+1 <=? Max x y) /~ True" $ assertCompileError source25 expected25

source26 :: String
source26 = [i|
{-# LANGUAGE TypeApplications #-}
|] <> preamble <> [i|

-- While n ~ (Max x y) implies x <= n (see test46), the reverse is not true.
test' :: ((x <=? n) ~ True)  => Proxy x -> Proxy y -> Proxy n -> Proxy ((Max x y)) -> Proxy n
test' _ _ _ = id

test = test' (Proxy @4) (Proxy @6) (Proxy @6)
|]

expected26 :: [String]
expected26 =
#if __GLASGOW_HASKELL__ >= 906
  ["Could not deduce 'Max x y ~ n'"
  ,"from the context: (x <=? n) ~ True"
  ]
#elif __GLASGOW_HASKELL__ <= 902
  ["Could not deduce: Max x y ~ n"
  ,"from the context: (x <=? n) ~ 'True"
  ]
#else
  ["Could not deduce (Max x y ~ n)"
  ,"from the context: (x <=? n) ~ 'True"
  ]
#endif

test26 :: TestTree
test26 = testCase "(x <= n) /=> (Max x y) ~ n" $ assertCompileError source26 expected26

source27 :: String
source27 = preamble <> [i|
test :: Proxy n -> Proxy (n + 2 <=? Max (n + 1) 1) -> Proxy True
test _ = id
|]

expected27 :: [String]
expected27 =
  ["Proxy ((n + 2) <=? Max (n + 1) 1) -> Proxy 'True"
  ,"Proxy 'True -> Proxy 'True"
  ]

test27 :: TestTree
test27 = testCase "n + 2 <=? Max (n + 1) 1 /~ True" $ assertCompileError source27 expected27

source28 :: String
source28 = [i|
{-# LANGUAGE KindSignatures #-}
|] <> preamble <> [i|
type Size (n :: Nat) = Max 0 (CLogWZ 2 n 0)

pack :: Proxy n -> Proxy (Size n)
pack _ = Proxy

repro :: Proxy (Size 1)
repro = pack (undefined :: Proxy (n :: Nat))
|]

expected28 :: [String]
expected28 =
  ["CLogWZ 2 n"
  ,"ambiguous"
  ]

test28 :: TestTree
test28 = testCase "CLogWZ reify with Max 0 (type synonym) doesn't panic" $
  assertCompileError source28 expected28
