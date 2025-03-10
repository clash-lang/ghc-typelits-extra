{-# LANGUAGE CPP, DataKinds, TypeOperators, TypeApplications, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE NoStarIsType #-}
#endif
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

import Data.List (isInfixOf)
import Data.Proxy
import Data.Type.Bool
import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit

import ErrorTests

import GHC.TypeLits
import GHC.TypeLits.Extra

test1 :: Proxy (GCD 6 8) -> Proxy 2
test1 = id

test2 :: Proxy ((GCD 6 8) + x) -> Proxy (x + (GCD 10 8))
test2 = id

test3 :: Proxy (CLog 3 10) -> Proxy 3
test3 = id

test4 :: Proxy ((CLog 3 10) + x) -> Proxy (x + (CLog 2 7))
test4 = id

test5 :: Proxy (CLog x (x^y)) -> Proxy y
test5 = id

test6 :: Integer
test6 = natVal (Proxy :: Proxy (CLog 6 8))

test7 :: Integer
test7 = natVal (Proxy :: Proxy (CLog 3 10))

test8 :: Integer
test8 = natVal (Proxy :: Proxy ((CLog 2 4) * (3 ^ (CLog 2 4))))

test9 :: Integer
test9 = natVal (Proxy :: Proxy (Max (CLog 2 4) (CLog 4 20)))

test10 :: Proxy (Div 9 3) -> Proxy 3
test10 = id

test11 :: Proxy (Div 9 4) -> Proxy 2
test11 = id

test12 :: Proxy (Mod 9 3) -> Proxy 0
test12 = id

test13 :: Proxy (Mod 9 4) -> Proxy 1
test13 = id

test14 :: Integer
test14 = natVal (Proxy :: Proxy (Div 9 3))

test15 :: Integer
test15 = natVal (Proxy :: Proxy (Mod 9 4))

test16 :: Proxy (LCM 18 7) -> Proxy 126
test16 = id

test17 :: Integer
test17 = natVal (Proxy :: Proxy (LCM 18 7))

test18 :: Proxy ((LCM 6 4) + x) -> Proxy (x + (LCM 3 4))
test18 = id

test19 :: Integer
test19 = natVal (Proxy :: Proxy (FLog 3 1))

test20 :: Proxy (FLog 3 1) -> Proxy 0
test20 = id

test21 :: Integer
test21 = natVal (Proxy :: Proxy (CLog 3 1))

test22 :: Proxy (CLog 3 1) -> Proxy 0
test22 = id

test23 :: Integer
test23 = natVal (Proxy :: Proxy (Log 3 1))

test24 :: Integer
test24 = natVal (Proxy :: Proxy (Log 3 9))

test25 :: Proxy (Log 3 9) -> Proxy 2
test25 = id

test26 :: Proxy (b ^ (Log b y)) -> Proxy y
test26 = id

test27 :: Proxy (Max n n) -> Proxy n
test27 = id

test28 :: Proxy (Min n n) -> Proxy n
test28 = id

test29 :: Proxy (Max n n + 1) -> Proxy (1 + n)
test29 = id

test30 :: Proxy n -> Proxy (1 + Max n n) -> Proxy (Min n n + 1)
test30 _ = id

test31 :: Proxy (Min n (n + 1)) -> Proxy n
test31 = id

test32 :: Proxy (Min (n + 1) n) -> Proxy n
test32 = id

test33 :: Proxy (Max n (n + 1)) -> Proxy (n+1)
test33 = id

test34 :: Proxy (Max (n + 1) n) -> Proxy (n+1)
test34 = id

test35 :: Proxy n -> Proxy (1 + Max n (1 + n)) -> Proxy (n + 2)
test35 _ = id

test36 :: Proxy n -> Proxy (1 + Min n (1 + n)) -> Proxy (n + 1)
test36 _ = id

test37 :: (1 <= Div l r) => Proxy l -> Proxy r -> ()
test37 _ _ = ()

test38 :: Proxy (Min (0-1) 0) -> Proxy (0-1)
test38 = id

test39 :: Proxy (Max (0-1) 0) -> Proxy (0-1)
test39 = id

test40 :: Proxy x -> Proxy y -> Proxy (Max x y) -> Proxy (Max y x)
test40 _ _ = id

test41 :: Proxy x -> Proxy y -> Proxy (Min x y) -> Proxy (Min y x)
test41 _ _ = id

test42 :: Proxy x -> Proxy y -> Proxy (GCD x y) -> Proxy (GCD y x)
test42 _ _ = id

test43 :: Proxy x -> Proxy y -> Proxy (LCM x y) -> Proxy (LCM y x)
test43 _ _ = id

test44 :: Proxy x -> Proxy y -> Proxy (x <=? (Max x y)) -> Proxy True
test44 _ _ = id

test45 :: Proxy x -> Proxy y -> Proxy (y <=? (Max x y)) -> Proxy True
test45 _ _ = id

test46 :: n ~ (Max x y) => Proxy x -> Proxy y -> Proxy (x <=? n) -> Proxy True
test46 _ _ = id

test47 :: n ~ (Max x y) => Proxy x -> Proxy y -> Proxy (y <=? n) -> Proxy True
test47 _ _ = id

test48
  :: Proxy n
  -> Proxy (Max (1+n) 1)
  -> Proxy (n+1)
test48 _ = id

test49
  :: Proxy n
  -> Proxy (Max (n+1) 1)
  -> Proxy (1+n)
test49 _ = id

test50
  :: Proxy n
  -> Proxy (Max (n+2) 1)
  -> Proxy (Max (2+n) 2)
test50 _ = id

test51
  :: Proxy n
  -> Proxy (Max (((2 ^ n) + 1) + ((2 ^ n) + 1)) 1)
  -> Proxy (2+((2^n)*2))
test51 _ = id

type family BitPack a :: Nat

test52
  :: Proxy a
  -> Proxy (1 + BitPack a)
  -> Proxy (Max 0 (BitPack a) + CLog 2 2)
test52 _ = id

test53
  :: Proxy n
  -> Proxy (1 <=? Max (n + 1) 1)
  -> Proxy True
test53 _ = id

test54
  :: Proxy n
  -> Proxy (n <=? Max (n + 1) 1)
  -> Proxy True
test54 _ = id

test55
  :: Proxy n
  -> Proxy (n + 1 <=? Max (n + 1) 1)
  -> Proxy True
test55 _ = id

test56
  :: Proxy n
  -> Proxy p
  -> Proxy (n <=? Max (n + p) p)
  -> Proxy True
test56 _ _ = id

test57
  :: Proxy n
  -> Proxy p
  -> Proxy (n + 1 <=? Max (n + p + 1) p)
  -> Proxy True
test57 _ _ = id

test58a
  :: 1 <= n
  => Proxy n
  -> Proxy n
test58a = id

test58b
  :: Proxy (Max (n+2) 1)
  -> Proxy (Max (n+2) 1)
test58b = test58a

test59 :: Proxy (CLogWZ 3 10 9) -> Proxy 3
test59 = id

test60 :: Proxy ((CLogWZ 3 10 3) + x) -> Proxy (x + (CLogWZ 2 7 8))
test60 = id

test61 :: Proxy (CLogWZ x (x^y) 8) -> Proxy y
test61 = id

test62 :: Integer
test62 = natVal (Proxy :: Proxy (CLogWZ 6 8 3))

test63 :: Integer
test63 = natVal (Proxy :: Proxy (CLogWZ 3 10 9))

test64 :: Integer
test64 = natVal (Proxy :: Proxy ((CLogWZ 2 4 11) * (3 ^ (CLogWZ 2 4 8))))

test65 :: Integer
test65 = natVal (Proxy :: Proxy (Max (CLogWZ 2 4 8) (CLogWZ 4 20 5)))

test66 :: Proxy (CLogWZ 3 0 8) -> Proxy 8
test66 = id

test67 :: Proxy (CLogWZ 2 0 x) -> Proxy x
test67 = id

test68 :: Proxy (CLogWZ 5 0 0) -> Proxy 0
test68 = id

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ghc-typelits-natnormalise"
  [ testGroup "Basic functionality"
    [ testCase "GCD 6 8 ~ 2" $
      show (test1 Proxy) @?=
      "Proxy"
    , testCase "forall x . GCD 6 8 + x ~ x + GCD 10 8" $
      show (test2 Proxy) @?=
      "Proxy"
    , testCase "CLog 3 10 ~ 3" $
      show (test3 Proxy) @?=
      "Proxy"
    , testCase "forall x . CLog 3 10 + x ~ x + CLog 2 7" $
      show (test4 Proxy) @?=
      "Proxy"
    , testCase "forall x>1 . CLog x (x^y) ~ y" $
      show (test5 Proxy) @?=
      "Proxy"
    , testCase "KnownNat (CLog 6 8) ~ 2" $
      show test6 @?=
      "2"
    , testCase "KnownNat (CLog 3 10) ~ 3" $
      show test7 @?=
      "3"
    , testCase "KnownNat ((CLog 2 4) * (3 ^ (CLog 2 4)))) ~ 18" $
      show test8 @?=
      "18"
    , testCase "KnownNat (Max (CLog 2 4) (CLog 4 20)) ~ 3" $
      show test9 @?=
      "3"
    , testCase "Div 9 3 ~ 3" $
      show (test10 Proxy) @?=
      "Proxy"
    , testCase "Div 9 4 ~ 2" $
      show (test11 Proxy) @?=
      "Proxy"
    , testCase "Mod 9 3 ~ 0" $
      show (test12 Proxy) @?=
      "Proxy"
    , testCase "Mod 9 4 ~ 1" $
      show (test13 Proxy) @?=
      "Proxy"
    , testCase "KnownNat (Div 9 3) ~ 3" $
      show test14 @?=
      "3"
    , testCase "KnownNat (Mod 9 4) ~ 1" $
      show test15 @?=
      "1"
    , testCase "LCM 18 7 ~ 126" $
      show (test16 Proxy) @?=
      "Proxy"
    , testCase "KnownNat (LCM 18 7) ~ 126" $
      show test17 @?=
      "126"
    , testCase "forall x . LCM 3 4 + x ~ x + LCM 6 4" $
      show (test18 Proxy) @?=
      "Proxy"
    , testCase "KnownNat (FLog 3 1) ~ 0" $
      show test19 @?=
      "0"
    , testCase "FLog 3 1 ~ 0" $
      show (test20 Proxy) @?=
      "Proxy"
    , testCase "KnownNat (CLog 3 1) ~ 0" $
      show test21 @?=
      "0"
    , testCase "CLog 3 1 ~ 0" $
      show (test22 Proxy) @?=
      "Proxy"
    , testCase "KnownNat (Log 3 1) ~ 0" $
      show test23 @?=
      "0"
    , testCase "KnownNat (Log 3 9) ~ 2" $
      show test24 @?=
      "2"
    , testCase "Log 3 9 ~ 2" $
      show (test25 Proxy) @?=
      "Proxy"
    , testCase "forall x>1 . x ^ (Log x y) ~ y" $
      show (test26 Proxy) @?=
      "Proxy"
    , testCase "forall x . Max x x ~ x" $
      show (test27 Proxy) @?=
      "Proxy"
    , testCase "forall x . Min x x ~ x" $
      show (test28 Proxy) @?=
      "Proxy"
    , testCase "forall x . (Max x x + 1) ~ (1 + x)" $
      show (test29 Proxy) @?=
      "Proxy"
    , testCase "forall x . (Min x x + 1) ~ (1 + Max x x)" $
      show (test30 Proxy Proxy) @?=
      "Proxy"
    , testCase "forall x . Min x (x+1) ~ x" $
      show (test31 Proxy) @?=
      "Proxy"
    , testCase "forall x . Min (x+1) x ~ x" $
      show (test32 Proxy) @?=
      "Proxy"
    , testCase "forall x . Max x (x+1) ~ (x+1)" $
      show (test33 Proxy) @?=
      "Proxy"
    , testCase "forall x . Max (x+1) x ~ (x+1)" $
      show (test34 Proxy) @?=
      "Proxy"
    , testCase "forall x . (1 + Max n (1+n)) ~ (2 + x)" $
      show (test35 Proxy Proxy) @?=
      "Proxy"
    , testCase "forall x . (1 + Min n (1+n)) ~ (1 + x)" $
      show (test36 Proxy Proxy) @?=
      "Proxy"
    , testCase "1 <= Div 18 3" $
      show (test37 (Proxy @18) (Proxy @3)) @?=
      "()"
    , testCase "Min (0-1) 0 ~ (0-1)" $
      show (test38 Proxy) @?=
      "Proxy"
    , testCase "Max (0-1) 0 ~ (0-1)" $
      show (test39 Proxy) @?=
      "Proxy"
    , testCase "forall x y . Max x y ~ Max y x" $
      show (test40 Proxy Proxy Proxy) @?=
      "Proxy"
    , testCase "forall x y . Min x y ~ Min y x" $
      show (test41 Proxy Proxy Proxy) @?=
      "Proxy"
    , testCase "forall x y . GCD x y ~ GCD y x" $
      show (test42 Proxy Proxy Proxy) @?=
      "Proxy"
    , testCase "forall x y . LCM x y ~ LCM y x" $
      show (test43 Proxy Proxy Proxy) @?=
      "Proxy"
    , testCase "forall x y . x <=? Max x y ~ True" $
      show (test44 Proxy Proxy Proxy) @?=
      "Proxy"
    , testCase "forall x y . y <=? Max x y ~ True" $
      show (test45 Proxy Proxy Proxy) @?=
      "Proxy"
    , testCase "forall x y n . n ~ Max x y => x <=? n ~ True" $
      show (test46 Proxy Proxy Proxy) @?=
      "Proxy"
    , testCase "forall x y n . n ~ Max x y => y <=? n ~ True" $
      show (test47 Proxy Proxy Proxy) @?=
      "Proxy"
    , testCase "forall n . Max (n+1) 1 ~ 1+n" $
      show (test48 Proxy Proxy) @?=
      "Proxy"
    , testCase "forall n . Max (1+n) 1 ~ n+1" $
      show (test49 Proxy Proxy) @?=
      "Proxy"
    , testCase "forall n . Max (n+2) 1 ~ Max (2+n) 2" $
      show (test50 Proxy Proxy) @?=
      "Proxy"
    , testCase "forall n . Max (((2 ^ n) + 1) + ((2 ^ n) + 1)) 1 ~ 2 + ((2 ^ n) * 2)" $
      show (test51 Proxy Proxy) @?=
      "Proxy"
    , testCase "forall a . (1 + BitPack a) ~ (Max 0 (BitPack a) + CLog 2 2" $
      show (test52 Proxy Proxy) @?=
      "Proxy"
    , testCase "forall n . 1 <= Max (n + 1) 1" $
      show (test53 Proxy Proxy) @?=
      "Proxy"
    , testCase "forall n . n <= Max (n + 1) 1" $
      show (test54 Proxy Proxy) @?=
      "Proxy"
    , testCase "forall n . n + 1 <= Max (n + 1) 1" $
      show (test55 Proxy Proxy) @?=
      "Proxy"
    , testCase "forall n p . n <= Max (n + p) p" $
      show (test56 Proxy Proxy Proxy) @?=
      "Proxy"
    , testCase "forall n p . n + 1 <= Max (n + p + 1) p" $
      show (test57 Proxy Proxy Proxy) @?=
      "Proxy"
    , testCase "CLogWZ 3 10 9 ~ 3" $
      show (test59 Proxy) @?=
      "Proxy"
    , testCase "forall x . CLogWZ 3 10 3 + x ~ x + CLogWZ 2 7 8" $
      show (test60 Proxy) @?=
      "Proxy"
    , testCase "forall x>1 . CLogWZ x (x^y) 8 ~ y" $
      show (test61 Proxy) @?=
      "Proxy"
    , testCase "KnownNat (CLogWZ 6 8 3) ~ 2" $
      show test62 @?=
      "2"
    , testCase "KnownNat (CLogWZ 3 10 9) ~ 3" $
      show test63 @?=
      "3"
    , testCase "KnownNat ((CLogWZ 2 4 11) * (3 ^ (CLogWZ 2 4 8)))) ~ 18" $
      show test64 @?=
      "18"
    , testCase "KnownNat (Max (CLogWZ 2 4 8) (CLogWZ 4 20 5)) ~ 3" $
      show test65 @?=
      "3"
    , testCase "CLogWZ 3 0 8 ~ 8" $
      show (test66 Proxy) @?=
      "Proxy"
    , testCase "forall x. CLogWZ 2 0 x ~ x" $
      show (test67 Proxy) @?=
      "Proxy"
    , testCase "CLogWZ 5 0 0 ~ 0" $
      show (test68 Proxy) @?=
      "Proxy"
    ]
  , testGroup "errors"
    [ testCase "GCD 6 8 /~ 4" $ testFail1 `throws` testFail1Errors
    , testCase "GCD 6 8 + x /~ x + GCD 9 6" $ testFail2 `throws` testFail2Errors
    , testCase "CLog 3 10 /~ 2" $ testFail3 `throws` testFail3Errors
    , testCase "CLog 3 10 + x /~ x + CLog 2 9" $ testFail4 `throws` testFail4Errors
    , testCase "CLog 0 4 /~ 100" $ testFail5 `throws` testFail5Errors
    , testCase "CLog 1 4 /~ 100" $ testFail5 `throws` testFail5Errors
    , testCase "CLog 4 0 /~ 0" $ testFail7 `throws` testFail7Errors
    , testCase "CLog 1 (1^y) /~ y" $ testFail8 `throws` testFail8Errors
    , testCase "CLog 0 (0^y) /~ y" $ testFail9 `throws` testFail9Errors
    , testCase "No instance (KnownNat (CLog 1 4))" $ testFail10 `throws` testFail10Errors
    , testCase "No instance (KnownNat (CLog 4 4 - CLog 2 4))" $ testFail11 `throws` testFail11Errors
    , testCase "Div 4 0 /~ 4" $ testFail12 `throws` testFail12Errors
    , testCase "Mod 4 0 /~ 4" $ testFail13 `throws` testFail13Errors
    , testCase "FLog 0 4 /~ 100" $ testFail14 `throws` testFail14Errors
    , testCase "FLog 1 4 /~ 100" $ testFail15 `throws` testFail15Errors
    , testCase "FLog 4 0 /~ 0" $ testFail16 `throws` testFail16Errors
    , testCase "GCD 6 8 /~ 4" $ testFail17 `throws` testFail17Errors
    , testCase "GCD 6 8 + x /~ x + GCD 9 6" $ testFail18 `throws` testFail18Errors
    , testCase "No instance (KnownNat (Log 3 0))" $ testFail19 `throws` testFail19Errors
    , testCase "No instance (KnownNat (Log 3 10))" $ testFail20 `throws` testFail20Errors
    , testCase "Min a (a*b) /~ a" $ testFail21 `throws` testFail21Errors
    , testCase "Max a (a*b) /~ (a*b)" $ testFail22 `throws` testFail22Errors
    , testCase "(1 <=? Div 18 6) ~ False" $ testFail23 `throws` testFail23Errors
    , testCase "(z <=? Max x y) /~ True" $ testFail24 `throws` testFail24Errors
    , testCase "(x+1 <=? Max x y) /~ True" $ testFail25 `throws` testFail25Errors
    , testCase "(x <= n) /=> (Max x y) ~ n" $ testFail26 `throws` testFail26Errors
    , testCase "n + 2 <=? Max (n + 1) 1 /~ True" $ testFail27 `throws` testFail27Errors
    ]
  ]

-- | Assert that evaluation of the first argument (to WHNF) will throw
-- an exception whose string representation contains the given
-- substrings.
throws :: a -> [String] -> Assertion
throws v xs = do
  result <- try (evaluate v)
  case result of
    Right _ -> assertFailure "No exception!"
    Left (TypeError msg) ->
      if all (`isInfixOf` (removeProblemChars msg)) $ map removeProblemChars xs
         then return ()
         else assertFailure msg

-- The kind and amount of quotes in GHC error messages changes depending on
-- whether or not our locale supports unicode.
-- Remove the problematic characters to enable comparison of errors.
removeProblemChars = filter (`notElem` problemChars)
  where problemChars = "‘’`'"
