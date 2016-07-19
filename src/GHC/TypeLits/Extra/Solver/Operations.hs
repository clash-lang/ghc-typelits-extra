{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, MagicHash #-}
#if __GLASGOW_HASKELL__ < 711
{-# LANGUAGE StandaloneDeriving #-}
#endif

module GHC.TypeLits.Extra.Solver.Operations
  ( ExtraOp (..)
  , EType (..)
  , mergeGCD
  , mergeCLog
  , mergeExp
  )
where

-- external
import GHC.Base               (isTrue#,(==#),(+#))
import GHC.Integer            (smallInteger)
import GHC.Integer.Logarithms (integerLogBase#)

-- GHC API
import Outputable (Outputable (..), (<+>), integer, text)
import Type       (Type, TyVar)
#if __GLASGOW_HASKELL__ >= 711
import Type (eqType)
#endif

newtype EType = EType Type
  deriving Outputable
#if __GLASGOW_HASKELL__ < 711
deriving instance Eq EType
#else
instance Eq EType where
  (EType t1) == (EType t2) = eqType t1 t2
#endif

data ExtraOp
  = I    Integer
  | V    TyVar
  | C    EType
  | GCD  ExtraOp ExtraOp
  | CLog ExtraOp ExtraOp
  | Exp  ExtraOp ExtraOp
  deriving Eq

instance Outputable ExtraOp where
  ppr (I i)      = integer i
  ppr (V v)      = ppr v
  ppr (C c)      = ppr c
  ppr (GCD x y)  = text "GCD (" <+> ppr x <+> text "," <+> ppr y <+> text ")"
  ppr (CLog x y) = text "CLog (" <+> ppr x <+> text "," <+> ppr y <+> text ")"
  ppr (Exp x y)  = text "Exp (" <+> ppr x <+> text "," <+> ppr y <+> text ")"

mergeGCD :: ExtraOp -> ExtraOp -> ExtraOp
mergeGCD (I i) (I j) = I (gcd i j)
mergeGCD x     y     = GCD x y

mergeCLog :: ExtraOp -> ExtraOp -> Maybe ExtraOp
mergeCLog i (Exp j k)
  | i == j && (i /= (I 0)) = Just k
mergeCLog (I i) (I j)      = I <$> clogBase i j
mergeCLog x y              = Just (CLog x y)

mergeExp :: ExtraOp -> ExtraOp -> ExtraOp
mergeExp (I i) (I j) = I (i^j)
mergeExp x     y     = Exp x y

-- | \x y -> ceiling (logBase x y), x > 1 && y > 0
clogBase :: Integer -> Integer -> Maybe Integer
clogBase x y | x > 1 && y > 0 =
  let z1 = integerLogBase# x y
      z2 = integerLogBase# x (y-1)
  in  if (isTrue# (z1 ==# z2))
         then Just (smallInteger (z1 +# 1#))
         else Just (smallInteger z1)
clogBase _ _ = Nothing
