{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module GHC.TypeLits.Extra where

import GHC.TypeLits

type family GCD (x :: Nat) (y :: Nat) :: Nat
