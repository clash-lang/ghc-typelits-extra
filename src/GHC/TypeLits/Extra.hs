{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module GHC.TypeLits.Extra where

import GHC.TypeLits

-- | Type-level greatest common denominator (GCD)
type family GCD (x :: Nat) (y :: Nat) :: Nat where
  GCD 0 x = x

-- | Type-level equivalent of:
--
-- @
-- clog x y = 'ceiling' ('logBase' x y)
-- @
type family CLog (x :: Nat) (y :: Nat) :: Nat where
  CLog 2 1 = 0
