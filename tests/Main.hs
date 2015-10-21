{-# LANGUAGE DataKinds, TypeOperators, ScopedTypeVariables #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

import Data.List (isInfixOf)
import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit

import ErrorTests

import GHC.TypeLits
import GHC.TypeLits.Extra
import Data.Proxy

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
      show (test4 Proxy) @?=
      "Proxy"
    ]
  , testGroup "errors"
    [ testCase "GCD 6 8 ~ 4" $ testFail1 `throws` testFail1Errors
    , testCase "GCD 6 8 + x ~ x + GCD 9 6" $ testFail2 `throws` testFail2Errors
    , testCase "CLog 3 10 ~ 2" $ testFail3 `throws` testFail3Errors
    , testCase "CLog 3 10 + x ~ x + CLog 2 9" $ testFail4 `throws` testFail4Errors
    , testCase "CLog 0 4 ~ 100" $ testFail5 `throws` testFail5Errors
    , testCase "CLog 1 4 ~ 100" $ testFail5 `throws` testFail5Errors
    , testCase "CLog 4 0 ~ 0" $ testFail7 `throws` testFail7Errors
    , testCase "CLog 1 (1^y) ~ y" $ testFail8 `throws` testFail8Errors
    , testCase "CLog 0 (0^y) ~ y" $ testFail9 `throws` testFail9Errors
    ]
  ]

-- | Assert that evaluation of the first argument (to WHNF) will throw
-- an exception whose string representation contains the given
-- substrings.
throws :: a -> [String] -> Assertion
throws v xs =
  (evaluate v >> assertFailure "No exception!")
  `catch` \ (e :: SomeException) -> if all (`isInfixOf` show e) xs then return () else throw e
