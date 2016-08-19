{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Additional type-level operations on 'GHC.TypeLits.Nat':

  * 'Max': type-level 'max'

  * 'Min': type-level 'min'

  * 'Div': type-level 'div'

  * 'Mod': type-level 'mod'

  * 'FLog': type-level equivalent of <https://hackage.haskell.org/package/integer-gmp/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35- integerLogBase#>

  * 'CLog': type-level equivalent of /the ceiling of/ <https://hackage.haskell.org/package/integer-gmp/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35- integerLogBase#>

  * 'GCD': a type-level 'gcd'

  * 'LCM': a type-level 'lcm'

A custom solver for the above operations defined is defined in
"GHC.TypeLits.Extra.Solver" as a GHC type-checker plugin. To use the plugin,
add the

@
{\-\# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver \#-\}
@

pragma to the header of your file.
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_HADDOCK show-extensions #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE Trustworthy #-}

module GHC.TypeLits.Extra
  ( -- * Type-level operations on `Nat`
    -- ** Order
    Max
  , Min
    -- ** Integral
  , Div
  , Mod
    -- ** Logarithm
  , FLog
  , CLog
    -- Numeric
  , GCD
  , LCM
  )
where

import Data.Proxy             (Proxy (..))
import Data.Singletons.TH     (genDefunSymbols)
import Data.Type.Bool         (If)
import GHC.Base               (isTrue#,(==#),(+#))
import GHC.Integer            (smallInteger)
import GHC.Integer.Logarithms (integerLogBase#)
import GHC.TypeLits           (KnownNat, Nat, type (<=), type (<=?), natVal)
import GHC.TypeLits.KnownNat  (KnownNat2 (..), SNatKn (..), nameToSymbol)

-- | Type-level 'max'
type family Max (x :: Nat) (y :: Nat) :: Nat where
  Max 0 y = y
  Max x y = If (x <=? y) y x

genDefunSymbols [''Max]

instance (KnownNat x, KnownNat y) => KnownNat2 $(nameToSymbol ''Max) x y where
  type KnownNatF2 $(nameToSymbol ''Max) = MaxSym0
  natSing2 = SNatKn (max (natVal (Proxy @x)) (natVal (Proxy @y)))

-- | Type-level 'min'
type family Min (x :: Nat) (y :: Nat) :: Nat where
  Min 0 y = 0
  Min x y = If (x <=? y) x y

genDefunSymbols [''Min]

instance (KnownNat x, KnownNat y) => KnownNat2 $(nameToSymbol ''Min) x y where
  type KnownNatF2 $(nameToSymbol ''Min) = MinSym0
  natSing2 = SNatKn (min (natVal (Proxy @x)) (natVal (Proxy @y)))

-- | Type-level 'div'
--
-- Note that additional equations are provided by the type-checker plugin solver
-- "GHC.TypeLits.Extra.Solver".
type family Div (x :: Nat) (y :: Nat) :: Nat where
  Div x 1 = x

genDefunSymbols [''Div]

instance (KnownNat x, KnownNat y, 1 <= y) => KnownNat2 $(nameToSymbol ''Div) x y where
  type KnownNatF2 $(nameToSymbol ''Div) = DivSym0
  natSing2 = SNatKn (div (natVal (Proxy @x)) (natVal (Proxy @y)))

-- | Type-level 'mod'
--
-- Note that additional equations are provided by the type-checker plugin solver
-- "GHC.TypeLits.Extra.Solver".
type family Mod (x :: Nat) (y :: Nat) :: Nat where
  Mod x 1 = 0

genDefunSymbols [''Mod]

instance (KnownNat x, KnownNat y, 1 <= y) => KnownNat2 $(nameToSymbol ''Mod) x y where
  type KnownNatF2 $(nameToSymbol ''Mod) = ModSym0
  natSing2 = SNatKn (mod (natVal (Proxy @x)) (natVal (Proxy @y)))

-- | Type-level equivalent of <https://hackage.haskell.org/package/integer-gmp/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35- integerLogBase#>
--
-- Note that additional equations are provided by the type-checker plugin solver
-- "GHC.TypeLits.Extra.Solver".
type family FLog (x :: Nat) (y :: Nat) :: Nat where
  FLog 2 1 = 0 -- Additional equations are provided by the custom solver

genDefunSymbols [''FLog]

instance (KnownNat x, KnownNat y, 2 <= x, 1 <= y) => KnownNat2 $(nameToSymbol ''FLog) x y where
  type KnownNatF2 $(nameToSymbol ''FLog) = FLogSym0
  natSing2 = SNatKn (smallInteger (integerLogBase# (natVal (Proxy @x)) (natVal (Proxy @y))))

-- | Type-level equivalent of /the ceiling of/ <https://hackage.haskell.org/package/integer-gmp/docs/GHC-Integer-Logarithms.html#v:integerLogBase-35- integerLogBase#>
--
-- Note that additional equations are provided by the type-checker plugin solver
-- "GHC.TypeLits.Extra.Solver".
type family CLog (x :: Nat) (y :: Nat) :: Nat where
  CLog 2 1 = 0 -- Additional equations are provided by the custom solver

genDefunSymbols [''CLog]

instance (KnownNat x, KnownNat y, 2 <= x, 1 <= y) => KnownNat2 $(nameToSymbol ''CLog) x y where
  type KnownNatF2 $(nameToSymbol ''CLog) = CLogSym0
  natSing2 = let x  = natVal (Proxy @x)
                 y  = natVal (Proxy @y)
                 z1 = integerLogBase# x y
                 z2 = integerLogBase# x (y-1)
             in  case y of
                    1 -> SNatKn 0
                    _ | isTrue# (z1 ==# z2) -> SNatKn (smallInteger (z1 +# 1#))
                      | otherwise           -> SNatKn (smallInteger z1)

-- | Type-level greatest common denominator (GCD).
--
-- Note that additional equations are provided by the type-checker plugin solver
-- "GHC.TypeLits.Extra.Solver".
type family GCD (x :: Nat) (y :: Nat) :: Nat where
  GCD 0 x = x -- Additional equations are provided by the custom solver

genDefunSymbols [''GCD]

instance (KnownNat x, KnownNat y) => KnownNat2 $(nameToSymbol ''GCD) x y where
  type KnownNatF2 $(nameToSymbol ''GCD) = GCDSym0
  natSing2 = SNatKn (gcd (natVal (Proxy @x)) (natVal (Proxy @y)))

-- | Type-level least common multiple (LCM).
--
-- Note that additional equations are provided by the type-checker plugin solver
-- "GHC.TypeLits.Extra.Solver".
type family LCM (x :: Nat) (y :: Nat) :: Nat where
  LCM 0 x = 0 -- Additional equations are provided by the custom solver

genDefunSymbols [''LCM]

instance (KnownNat x, KnownNat y) => KnownNat2 $(nameToSymbol ''LCM) x y where
  type KnownNatF2 $(nameToSymbol ''LCM) = LCMSym0
  natSing2 = SNatKn (lcm (natVal (Proxy @x)) (natVal (Proxy @y)))
