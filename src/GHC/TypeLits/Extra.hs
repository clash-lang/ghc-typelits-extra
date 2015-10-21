{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE Safe #-}

{-|
Copyright  :  (C) 2015, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>

Additional type-level operations on 'GHC.TypeLits.Nat':

  * 'GCD': a type-level 'gcd'

  * 'CLog': type-level equivalent of
    \"@clog x y = 'ceiling' ('logBase' x y)@\"

A custom solver for the above operations defined is defined in
"GHC.TypeLits.Extra.Solver" as a GHC type-checker plugin. To use the plugin,
add the

@
{\-\# OPTIONS_GHC -fplugin GHC.TypeLits.Extra.Solver \#-\}
@

pragma to the header of your file.
-}
module GHC.TypeLits.Extra where

import GHC.TypeLits (Nat)

-- | Type-level greatest common denominator (GCD).
--
-- Note that additional equations are provided by the type-checker plugin solver
-- "GHC.TypeLits.Extra.Solver".
type family GCD (x :: Nat) (y :: Nat) :: Nat where
  GCD 0 x = x -- Additional equations are provided by the custom solver

-- | Type-level equivalent of:
--
-- @
-- clog x y = 'ceiling' ('logBase' x y)
-- @
--
-- Note that additional equations are provided by the type-checker plugin solver
-- "GHC.TypeLits.Extra.Solver".
type family CLog (x :: Nat) (y :: Nat) :: Nat where
  CLog 2 1 = 0 -- Additional equations are provided by the custom solver
