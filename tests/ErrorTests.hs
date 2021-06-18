{-# LANGUAGE CPP, DataKinds, TypeOperators, TypeApplications, TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE NoStarIsType #-}
#endif
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

testFail21 :: Proxy a -> Proxy b -> Proxy (Min a (a*b)) -> Proxy a
testFail21 _ _ = id

testFail22 :: Proxy a -> Proxy b -> Proxy (Max a (a*b)) -> Proxy (a*b)
testFail22 _ _ = id

testFail23' :: ((1 <=? Div l r) ~ False) => Proxy l -> Proxy r -> ()
testFail23' _ _ = ()

testFail23 :: ()
testFail23 = testFail23' (Proxy @18) (Proxy @3)

testFail24 :: Proxy x -> Proxy y -> Proxy z -> Proxy (z <=? Max x y) -> Proxy True
testFail24 _ _ _ = id

testFail25 :: Proxy x -> Proxy y -> Proxy (x+1 <=? Max x y) -> Proxy True
testFail25 _ _ = id

-- While n ~ (Max x y) implies x <= n (see test46), the reverse is not true.
testFail26' :: ((x <=? n) ~ True)  => Proxy x -> Proxy y -> Proxy n -> Proxy ((Max x y)) -> Proxy n
testFail26' _ _ _ = id

testFail26 = testFail26' (Proxy @4) (Proxy @6) (Proxy @6)

testFail27 :: Proxy n -> Proxy (n + 2 <=? Max (n + 1) 1) -> Proxy True
testFail27 _ = id

#if __GLASGOW_HASKELL__ >= 900
testFail1Errors =
  ["Expected: Proxy (GCD 6 8) -> Proxy 4"
  ,"  Actual: Proxy 4 -> Proxy 4"
  ]

testFail2Errors =
  ["Expected: Proxy (GCD 6 8 + x) -> Proxy (x + GCD 6 9)"
  ,"  Actual: Proxy (GCD 6 8 + x) -> Proxy (GCD 6 8 + x)"
  ]

testFail3Errors =
  ["Expected: Proxy (CLog 3 10) -> Proxy 2"
  ,"  Actual: Proxy 2 -> Proxy 2"
  ]

testFail4Errors =
  ["Expected: Proxy (CLog 3 10 + x) -> Proxy (x + CLog 2 9)"
  ,"  Actual: Proxy (CLog 3 10 + x) -> Proxy (CLog 3 10 + x)"
  ]

testFail5Errors =
  ["Expected: Proxy (CLog 0 4) -> Proxy 100"
  ,"  Actual: Proxy 100 -> Proxy 100"
  ]

testFail6Errors =
  ["Expected: Proxy (CLog 1 4) -> Proxy 100"
  ,"  Actual: Proxy 100 -> Proxy 100"
  ]

testFail7Errors =
  ["Expected: Proxy (CLog 4 0) -> Proxy 0"
  ,"  Actual: Proxy 0 -> Proxy 0"
  ]

testFail8Errors =
  ["Expected: Proxy (CLog 1 (1 ^ y)) -> Proxy y"
  ,"  Actual: Proxy y -> Proxy y"
  ]

testFail9Errors =
  ["Expected: Proxy (CLog 0 (0 ^ y)) -> Proxy y"
  ,"  Actual: Proxy y -> Proxy y"
  ]

testFail12Errors =
  ["Expected: Proxy (Div 4 0) -> Proxy 4"
  ,"  Actual: Proxy 4 -> Proxy 4"
  ]

testFail13Errors =
  ["Expected: Proxy (Mod 4 0) -> Proxy 4"
  ,"  Actual: Proxy 4 -> Proxy 4"
  ]

testFail14Errors =
  ["Expected: Proxy (FLog 0 4) -> Proxy 100"
  ,"  Actual: Proxy 100 -> Proxy 100"
  ]

testFail15Errors =
  ["Expected: Proxy (FLog 1 4) -> Proxy 100"
  ,"  Actual: Proxy 100 -> Proxy 100"
  ]

testFail16Errors =
  ["Expected: Proxy (FLog 4 0) -> Proxy 0"
  ,"  Actual: Proxy 0 -> Proxy 0"
  ]

testFail17Errors =
  ["Expected: Proxy (LCM 6 8) -> Proxy 48"
  ,"  Actual: Proxy 48 -> Proxy 48"
  ]

testFail18Errors =
  ["Expected: Proxy (LCM 6 8 + x) -> Proxy (x + LCM 6 9)"
  ,"  Actual: Proxy (LCM 6 8 + x) -> Proxy (LCM 6 8 + x)"
  ]

testFail19Errors =
  ["Couldn't match type: FLog 3 0"
  ,"               with: CLog 3 0"]

testFail20Errors =
  ["Couldn't match type: FLog 3 10"
  ,"               with: CLog 3 10"]

testFail21Errors =
  ["Expected: Proxy (Min a (a * b)) -> Proxy a"
  ,"  Actual: Proxy a -> Proxy a"
  ]

testFail22Errors =
  ["Expected: Proxy (Max a (a * b)) -> Proxy (a * b)"
  ,"  Actual: Proxy (Max a (a * b)) -> Proxy (Max a (a * b))"]

testFail27Errors =
  ["Expected: Proxy ((n + 2) <=? Max (n + 1) 1) -> Proxy 'True"
  ,"  Actual: Proxy 'True -> Proxy 'True"
  ]
#else
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

testFail21Errors =
  ["Expected type: Proxy (Min a (a * b)) -> Proxy a"
  ,"Actual type: Proxy a -> Proxy a"
  ]

testFail22Errors =
  ["Expected type: Proxy (Max a (a * b)) -> Proxy (a * b)"
  ,"Actual type: Proxy (a * b) -> Proxy (a * b)"]

testFail27Errors =
  ["Expected type: Proxy ((n + 2) <=? Max (n + 1) 1) -> Proxy 'True"
  ,"Actual type: Proxy 'True -> Proxy 'True"
  ]
#endif

testFail10Errors =
  ["Couldn't match type ‘'False’ with ‘'True’"]

testFail11Errors =
#if __GLASGOW_HASKELL__ >= 902
  ["Couldn't match type ‘Data.Type.Ord.OrdCond"
  ,"(CmpNat (CLog 2 4) (CLog 4 4)) 'True 'True 'False’"
  ,"with ‘'True’"]
#else
  ["Couldn't match type ‘CLog 2 4 <=? CLog 4 4’ with ‘'True’"]
#endif

testFail23Errors =
#if __GLASGOW_HASKELL__ >= 804
  ["Couldn't match type ‘'True’ with ‘'False’"]
#else
  ["Couldn't match type ‘1 <=? Div 18 3’ with ‘'False’"]
#endif

testFail24Errors =
#if __GLASGOW_HASKELL__ >= 902
  ["Couldn't match type ‘Data.Type.Ord.OrdCond"
  ,"(CmpNat z (Max x y)) 'True 'True 'False’"
  ,"with ‘'True’"]
#else
  ["Couldn't match type ‘z <=? Max x y’ with ‘'True’"]
#endif

testFail25Errors =
#if __GLASGOW_HASKELL__ >= 902
  ["Couldn't match type ‘Data.Type.Ord.OrdCond"
  ,"(CmpNat (x + 1) (Max x y)) 'True 'True 'False’"
  ,"with ‘'True’"]
#else
  ["Couldn't match type ‘(x + 1) <=? Max x y’ with ‘'True’"]
#endif

testFail26Errors =
  ["Could not deduce: Max x y ~ n"
  ,"from the context: (x <=? n) ~ 'True"
  ]
