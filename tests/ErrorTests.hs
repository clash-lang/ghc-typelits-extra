{-# LANGUAGE DataKinds, TypeOperators #-}

{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module ErrorTests where

import GHC.TypeLits
import GHC.TypeLits.Extra
import Data.Proxy

testFail1 :: Proxy (GCD 6 8) -> Proxy 4
testFail1 = id

testFail2 :: Proxy ((GCD 6 8) + x) -> Proxy (x + (GCD 6 9))
testFail2 = id

testFail1Errors =
  ["Expected type: Proxy (GCD 6 8) -> Proxy 4"
  ,"Actual type: Proxy 4 -> Proxy 4"
  ]

testFail2Errors =
  ["Expected type: Proxy (GCD 6 8 + x) -> Proxy (x + GCD 6 9)"
  ,"Actual type: Proxy (x + GCD 6 9) -> Proxy (x + GCD 6 9)"
  ]
