module GHC.TypeLits.Extra.Solver.Operations where

-- GHC API
import Outputable (Outputable (..), (<+>), integer, text)
import Type       (TyVar)

data ExtraOp
  = I   Integer
  | V   TyVar
  | GCD ExtraOp ExtraOp
  deriving Eq

instance Outputable ExtraOp where
  ppr (I i)     = integer i
  ppr (V v)     = ppr v
  ppr (GCD x y) = text "GCD (" <+> ppr x <+> text "," <+> ppr y <+> text ")"

mergeGCD :: ExtraOp -> ExtraOp -> ExtraOp
mergeGCD (I i) (I j) = I (gcd i j)
mergeGCD x     y     = GCD x y
