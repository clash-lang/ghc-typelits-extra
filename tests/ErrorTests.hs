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

testFail3 :: Proxy (CLog 3 10) -> Proxy 2
testFail3 = id

testFail4 :: Proxy ((CLog 3 10) + x) -> Proxy (x + (CLog 2 9))
testFail4 = id

testFail5 :: Proxy (CLog 0 4) -> Proxy 100
testFail5 = id

testFail6 :: Proxy (CLog 1 4) -> Proxy 100
testFail6 = id

testFail7 :: Proxy (CLog 4 0) -> Proxy 0
testFail7 = id

testFail1Errors =
  ["Expected type: Proxy (GCD 6 8) -> Proxy 4"
  ,"Actual type: Proxy 4 -> Proxy 4"
  ]

testFail2Errors =
  ["Expected type: Proxy (GCD 6 8 + x) -> Proxy (x + GCD 6 9)"
  ,"Actual type: Proxy (x + GCD 6 9) -> Proxy (x + GCD 6 9)"
  ]

testFail3Errors =
  ["Expected type: Proxy (CLog 3 10) -> Proxy 2"
  ,"Actual type: Proxy 2 -> Proxy 2"
  ]

testFail4Errors =
  ["Expected type: Proxy (CLog 3 10 + x) -> Proxy (x + CLog 2 9)"
  ,"Actual type: Proxy (x + CLog 2 9) -> Proxy (x + CLog 2 9)"
  ]

testFail5Errors =
  ["Expected type: Proxy (CLog 0 4) -> Proxy 100"
  ,"Actual type: Proxy 100 -> Proxy 100"
  ]

testFail6Errors =
  ["Expected type: Proxy (CLog 1 4) -> Proxy 100"
  ,"Actual type: Proxy 100 -> Proxy 100"
  ]

testFail7Errors =
  ["Expected type: Proxy (CLog 4 0) -> Proxy 0"
  ,"Actual type: Proxy 0 -> Proxy 0"
  ]
