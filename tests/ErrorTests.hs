{-# LANGUAGE DataKinds, TypeOperators #-}

{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver #-}

module ErrorTests where

import Data.Proxy
import GHC.TypeLits
import GHC.TypeLits.Extra

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

testFail8 :: Proxy (CLog 1 (1^y)) -> Proxy y
testFail8 = id

testFail9 :: Proxy (CLog 0 (0^y)) -> Proxy y
testFail9 = id

testFail10 :: Integer
testFail10 = natVal (Proxy :: Proxy (CLog 1 4))

testFail11 :: Integer
testFail11 = natVal (Proxy :: Proxy ((CLog 4 4) - (CLog 2 4)))

testFail12 :: Proxy (Div 4 0) -> Proxy 4
testFail12 = id

testFail13 :: Proxy (Mod 4 0) -> Proxy 4
testFail13 = id

testFail14 :: Proxy (FLog 0 4) -> Proxy 100
testFail14 = id

testFail15 :: Proxy (FLog 1 4) -> Proxy 100
testFail15 = id

testFail16 :: Proxy (FLog 4 0) -> Proxy 0
testFail16 = id

testFail17 :: Proxy (LCM 6 8) -> Proxy 48
testFail17 = id

testFail18 :: Proxy ((LCM 6 8) + x) -> Proxy (x + (LCM 6 9))
testFail18 = id

testFail19 :: Integer
testFail19 = natVal (Proxy :: Proxy (Log 3 0))

testFail20 :: Integer
testFail20 = natVal (Proxy :: Proxy (Log 3 10))

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

testFail8Errors =
  ["Expected type: Proxy (CLog 1 (1 ^ y)) -> Proxy y"
  ,"Actual type: Proxy y -> Proxy y"
  ]

testFail9Errors =
  ["Expected type: Proxy (CLog 0 (0 ^ y)) -> Proxy y"
  ,"Actual type: Proxy y -> Proxy y"
  ]

testFail10Errors =
  ["Couldn't match type ‘'False’ with ‘'True’"]

testFail11Errors =
  ["Couldn't match type ‘CLog 2 4 <=? CLog 4 4’ with ‘'True’"]

testFail12Errors =
  ["Expected type: Proxy (Div 4 0) -> Proxy 4"
  ,"Actual type: Proxy 4 -> Proxy 4"
  ]

testFail13Errors =
  ["Expected type: Proxy (Mod 4 0) -> Proxy 4"
  ,"Actual type: Proxy 4 -> Proxy 4"
  ]

testFail14Errors =
  ["Expected type: Proxy (FLog 0 4) -> Proxy 100"
  ,"Actual type: Proxy 100 -> Proxy 100"
  ]

testFail15Errors =
  ["Expected type: Proxy (FLog 1 4) -> Proxy 100"
  ,"Actual type: Proxy 100 -> Proxy 100"
  ]

testFail16Errors =
  ["Expected type: Proxy (FLog 4 0) -> Proxy 0"
  ,"Actual type: Proxy 0 -> Proxy 0"
  ]

testFail17Errors =
  ["Expected type: Proxy (LCM 6 8) -> Proxy 48"
  ,"Actual type: Proxy 48 -> Proxy 48"
  ]

testFail18Errors =
  ["Expected type: Proxy (LCM 6 8 + x) -> Proxy (x + LCM 6 9)"
  ,"Actual type: Proxy (x + LCM 6 9) -> Proxy (x + LCM 6 9)"
  ]

testFail19Errors =
  ["Couldn't match type ‘FLog 3 0’ with ‘CLog 3 0’"]

testFail20Errors =
  ["Couldn't match type ‘FLog 3 10’ with ‘CLog 3 10’"]
