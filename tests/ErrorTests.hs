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
#if __GLASGOW_HASKELL__ >= 901
import qualified Data.Type.Ord
#endif

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

testFail1Errors =
  ["Proxy (GCD 6 8) -> Proxy 4"
  ,"Proxy 4 -> Proxy 4"
  ]

testFail2Errors =
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

testFail3Errors =
  ["Proxy (CLog 3 10) -> Proxy 2"
  ,"Proxy 2 -> Proxy 2"
  ]

testFail4Errors =
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

testFail5Errors =
  ["Proxy (CLog 0 4) -> Proxy 100"
  ,"Proxy 100 -> Proxy 100"
  ]

testFail6Errors =
  ["Proxy (CLog 1 4) -> Proxy 100"
  ,"Proxy 100 -> Proxy 100"
  ]

testFail7Errors =
  ["Proxy (CLog 4 0) -> Proxy 0"
  ,"Proxy 0 -> Proxy 0"
  ]

testFail8Errors =
  ["Proxy (CLog 1 (1 ^ y)) -> Proxy y"
  ,"Proxy y -> Proxy y"
  ]

testFail9Errors =
  ["Proxy (CLog 0 (0 ^ y)) -> Proxy y"
  ,"Proxy y -> Proxy y"
  ]

testFail10Errors =
#if __GLASGOW_HASKELL__ >= 904
  ["Cannot satisfy: 2 <= 1"]
#else
  ["Couldn't match type ‘'False’ with ‘'True’"]
#endif

testFail11Errors =
#if __GLASGOW_HASKELL__ >= 904
  ["Cannot satisfy: 2 <= 1"]
#else
  ["Couldn't match type ‘'False` with ‘'True’"]
#endif

testFail12Errors =
  ["Proxy (Div 4 0) -> Proxy 4"
  ,"Proxy 4 -> Proxy 4"
  ]

testFail13Errors =
  ["Proxy (Mod 4 0) -> Proxy 4"
  ,"Proxy 4 -> Proxy 4"
  ]

testFail14Errors =
  ["Proxy (FLog 0 4) -> Proxy 100"
  ,"Proxy 100 -> Proxy 100"
  ]

testFail15Errors =
  ["Proxy (FLog 1 4) -> Proxy 100"
  ,"Proxy 100 -> Proxy 100"
  ]

testFail16Errors =
  ["Proxy (FLog 4 0) -> Proxy 0"
  ,"Proxy 0 -> Proxy 0"
  ]

testFail17Errors =
  ["Proxy (LCM 6 8) -> Proxy 48"
  ,"Proxy 48 -> Proxy 48"
  ]

testFail18Errors =
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

testFail19Errors =
#if __GLASGOW_HASKELL__ >= 900
  ["Couldn't match type: FLog 3 0"
  ,"               with: CLog 3 0"]
#else
  ["Couldn't match type ‘FLog 3 0’ with ‘CLog 3 0’"]
#endif

testFail20Errors =
#if __GLASGOW_HASKELL__ >= 904
  ["Couldn't match type ‘1’ with ‘0’"]
#elif __GLASGOW_HASKELL__ >= 900
  ["Couldn't match type: FLog 3 10"
  ,"               with: CLog 3 10"]
#else
  ["Couldn't match type ‘FLog 3 10’ with ‘CLog 3 10’"]
#endif

testFail21Errors =
  ["Proxy (Min a (a * b)) -> Proxy a"
  ,"Proxy a -> Proxy a"
  ]

testFail22Errors =
#if __GLASGOW_HASKELL__ >= 900
  ["Proxy (Max a (a * b)) -> Proxy (a * b)"
  ,"Proxy (Max a (a * b)) -> Proxy (Max a (a * b))"]
#else
  ["Proxy (Max a (a * b)) -> Proxy (a * b)"
  ,"Proxy (a * b) -> Proxy (a * b)"]
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
#if __GLASGOW_HASKELL__ >= 906
  ["Could not deduce ‘Max x y ~ n’"
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

testFail27Errors =
  ["Proxy ((n + 2) <=? Max (n + 1) 1) -> Proxy 'True"
  ,"Proxy 'True -> Proxy 'True"
  ]
