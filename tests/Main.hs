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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "ghc-typelits-natnormalise"
  [ testGroup "Basic functionality"
    [ testCase "GCD 6 8 ~ 2" $
      show (test1 Proxy) @?=
      "Proxy"
    , testCase "GCD 6 8 + x ~ x + GCD 10 8" $
      show (test2 Proxy) @?=
      "Proxy"
    ]
  , testGroup "errors"
    [ testCase "GCD 6 8 ~ 4" $ testFail1 `throws` testFail1Errors
    , testCase "GCD 6 8 + x ~ x + GCD 9 6" $ testFail2 `throws` testFail2Errors
    ]
  ]

-- | Assert that evaluation of the first argument (to WHNF) will throw
-- an exception whose string representation contains the given
-- substrings.
throws :: a -> [String] -> Assertion
throws v xs =
  (evaluate v >> assertFailure "No exception!")
  `catch` \ (e :: SomeException) -> if all (`isInfixOf` show e) xs then return () else throw e
