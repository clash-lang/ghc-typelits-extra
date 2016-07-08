{-|
Copyright  :  (C) 2015-2016, University of Twente
License    :  BSD2 (see the file LICENSE)
Maintainer :  Christiaan Baaij <christiaan.baaij@gmail.com>
-}

module GHC.TypeLits.Extra.Solver.Operations
  ( ExtraOp (..)
  , mergeGCD
  , mergeCLog
  , mergeExp
  , mergeAdd
  , mergeSub
  , mergeMul
  )
where

-- GHC API
import Outputable (Outputable (..), (<+>), integer, text)
import Type       (Type, TyVar, mkNumLitTy)

data ExtraOp
  = I    Integer
  | V    TyVar
  | C    Type
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
mergeCLog (I i) (I j)
  | i > 1 && j > 0 = Just (I (ceiling (logBase (fromInteger i :: Double)
                                      (fromInteger j))))
  | otherwise      = Nothing
mergeCLog x y = Just (CLog x y)

mergeExp :: ExtraOp -> ExtraOp -> ExtraOp
mergeExp (I i) (I j) = I (i^j)
mergeExp x     y     = Exp x y

mergeAdd :: ExtraOp -> ExtraOp -> Maybe ExtraOp
mergeAdd (I i) (I j) = Just (I (i + j))
mergeAdd _     _     = Nothing

mergeSub :: ExtraOp -> ExtraOp -> Maybe ExtraOp
mergeSub (I i) (I j)
  | let s = i - j
  , s >= 0 = Just (I s)
mergeSub _     _     = Nothing

mergeMul :: ExtraOp -> ExtraOp -> Maybe ExtraOp
mergeMul (I i) (I j) = Just (I (i * j))
mergeMul _     _     = Nothing
