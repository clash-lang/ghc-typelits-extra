{-# LANGUAGE DataKinds, TypeOperators #-}

{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module ErrorTests where

import GHC.TypeLits
import GHC.TypeLits.Extra
import Data.Proxy

testFail1 :: Proxy (GCD 6 8) -> Proxy 3
testFail1 = id

testFail2 :: Proxy ((GCD 6 8) + x) -> Proxy (x + 3)
testFail2 = id

testFail1Errors =
  ["Expected type: Proxy (GCD 6 8) -> Proxy 3"
  ,"Actual type: Proxy 3 -> Proxy 3"
  ]

testFail2Errors =
  ["Expected type: Proxy (GCD 6 8 + x) -> Proxy (x + 3)"
  ,"Actual type: Proxy (x + 3) -> Proxy (x + 3)"
  ]
